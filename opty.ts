#!/usr/bin/env node

import { spawn } from 'child_process';
import { readFileSync } from 'fs';
import Lodash from 'lodash';
import {
  properSubset,
  subset,
} from 'set-utilities';
import {
  Readable,
  Writable,
} from 'stream';
import yargs from 'yargs/yargs';

import { particleSwarmOptimize } from './pso.js';

const { zipWith } = Lodash;



export type ParticlePosition = Array<number>
type ParticleHeader = Set<string>
export type PositionPacket = ParticlePosition[]
export type ValuePacket = number[]

function readParticlesFile(filename: string) {
  const configData = readFileSync(filename, {encoding: "utf-8"});
  const particles: Array<{[key: string]: number}> = JSON.parse(configData);
  
  function setEqual(setA: Set<string>, setB: Set<string>) {
    return subset(setA, setB) && ! properSubset(setA, setB);
  }
  
  let globalHeaders: Set<string> = new Set();
  let isFirstParticle = true;
  const particlePositions: Array<ParticlePosition> = [];
  for (const particle of particles) {
    const particleHeaders = new Set(Object.keys(particle));
    if (isFirstParticle) {
      globalHeaders = particleHeaders;
      isFirstParticle = false;
    }
    else {
      if (! setEqual(globalHeaders, particleHeaders)) {
        throw new Error("error: particle headers are inconsistent")
      }
    }
    particlePositions.push(Object.values(particle));
  }
  
  return {
    header: globalHeaders,
    positions: particlePositions
  }
}

function serializeParticle(header: ParticleHeader, position: ParticlePosition) {
  return zipWith(
    Array.from(header), 
    position, 
    (variableName, coordinate) => `${variableName}=${coordinate}`
  ).join(' ') + "\n";
}

function writeParticle(
  position: ParticlePosition, 
  header: ParticleHeader, 
  outputStream: Writable
) {
  //await waitForPipe(outputStream)
  outputStream.write(serializeParticle(header, position));
}

function waitForEnd(stream: Readable): Promise<void> {
  return new Promise((resolve, reject) => {
    stream.once("end", () => resolve())
  })
}

function runChild(
  command: string, 
  args: Array<string>, 
  particleHeader: ParticleHeader,
  negate:boolean = false
) {
  
  return async (particlePositions: PositionPacket): Promise<ValuePacket> => {
    const child = spawn(command, args, {stdio: ["pipe", "pipe", "inherit"]});
    
    for (const position of particlePositions) {
      child.stdin.write(serializeParticle(particleHeader, position))
    }

    let data = ""
    child.stdout.on("data", 
      (chunk) => {
        data += chunk.toString()
      })
    child.stdin.end()
    await waitForEnd(child.stdout);

    const lines = data.trim().split("\n")
    const values = lines.map((line) => parseFloat(line) * (negate ? -1 : 1))
    return values;
  }
}

const argv = yargs(process.argv.slice(2)).options({
  maximize: { type: 'boolean', default: false, alias: "M"},
  numIterations: { type: 'number', default: 100, alias: "n"},
  initialPositions: { type: 'string', demandOption: true, alias: "g"},
  numWorkers: { type: 'number', default: 16, alias: "j"},
  workPacketSize: { type: 'number', default: 10, alias: "p"}
}).parseSync();

async function main() {
  const particles = readParticlesFile(argv.initialPositions);
  const fullCommand = argv._.map((arg) => arg.toString());
  const command = fullCommand[0];
  const commandArgs = fullCommand.slice(1);
  const objectiveFunction = runChild(
    command, 
    commandArgs, 
    particles.header, 
    argv.maximize,
  );
  const optimizedPoint = await particleSwarmOptimize(
    particles.positions, 
    objectiveFunction, 
    argv.numIterations, 
    argv.numWorkers,
    argv.workPacketSize
  );
  writeParticle(optimizedPoint.position, particles.header, process.stdout);
}

main();

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

const argv = yargs(process.argv.slice(2)).options({
  M: { type: 'boolean', default: false},
  n: { type: 'number', default: 100, alias: "num-iterations"},
  g: { type: 'string', demandOption: true, alias: "initial-positions"},
  j: { type: 'number', default: 16, alias: "num-workers"}
}).parseSync();

export type ParticlePosition = Array<number>
type ParticleHeader = Set<string>

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

function writeParticle(
  position: ParticlePosition, 
  header: ParticleHeader, 
  outputStream: Writable
) {
  const serializedParticle = zipWith(
    Array.from(header), 
    position, 
    (variableName, coordinate) => `${variableName}=${coordinate}`
  ).join(' ') + "\n";
  outputStream.write(serializedParticle);
}

function runChild(
  command: string, 
  args: Array<string>, 
  particleHeader: ParticleHeader,
  negate:boolean = false
) {
  const readFunctionValue = async (inputStream: Readable) => {
    const resultText: string = await inputStream.reduce(
      (accumulatedLine: string, chunk) => {
      accumulatedLine += chunk.toString();
      return accumulatedLine
    }, "")
    const parsedValue = Number(resultText.split("\n")[0]);
    return parsedValue * (negate ? -1 : 1);
  }
  
  return async (particlePosition: ParticlePosition) => {
    const child = spawn(command, args, {stdio: ["pipe", "pipe", "inherit"]});
    writeParticle(particlePosition, particleHeader, child.stdin);
    child.stdin.end();
    return await readFunctionValue(child.stdout);
  }
}

async function main() {
  const particles = readParticlesFile(argv.g);
  const fullCommand = argv._.map((arg) => arg.toString());
  const command = fullCommand[0];
  const commandArgs = fullCommand.slice(1);
  const objectiveFunction = runChild(
    command, 
    commandArgs, 
    particles.header, 
    argv.M
  );
  const optimizedPoint = await particleSwarmOptimize(
    particles.positions, 
    objectiveFunction, 
    argv.n, 
    argv.j
  );
  writeParticle(optimizedPoint.position, particles.header, process.stdout);
}

main();

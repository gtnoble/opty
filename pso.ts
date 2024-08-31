import assert from 'assert/strict';
import Lodash from 'lodash';
import pLimit from 'p-limit';

import { ParticlePosition } from './opty.js';

const { zipWith } = Lodash

const square = (x: number) => Math.pow(x, 2);

type ObjectiveFunction = (position: ParticlePosition[]) => Promise<number[]>;

interface ParticleState {
  position: ParticlePosition,
  energy: number
}

function sampleNormalDistribution1D(mean: number, standardDeviation: number) {
  const sample = 
    mean +
    (Math.cos(2 * Math.PI * Math.random()) *
    Math.sqrt(-2 * Math.log(Math.random())) *
    standardDeviation);
  return sample;
}

function selectOptimumEnergyState(
  state1: ParticleState, 
  state2: ParticleState
) {
  if (isNaN(state1.energy) && ! isNaN(state2.energy)) {
    return state2;
  }
  else if (! isNaN(state1.energy) && isNaN(state2.energy)) {
    return state1;
  }
  else if (state1.energy < state2.energy) {
    return state1;
  }
  else {
    return state2;
  }
}

function vectorNorm(vector: ParticlePosition) {
  return Math.sqrt(
    vector.reduce(
      (sum, next) => sum + square(next),
      0
    )
  );
}

function vectorAdd(vector1: ParticlePosition, vector2: ParticlePosition) {
  return zipWith(vector1, vector2, (element1, element2) => element1 + element2);
}

function vectorSubtract(vector1: ParticlePosition, vector2: ParticlePosition) {
  return zipWith(vector1, vector2, (element1, element2) => element1 - element2);
}

function vectorScale(scalar: number, vector: ParticlePosition) {
  return vector.map((element) => element * scalar);
}

function getBestState(particleStates: Array<ParticleState>) {
  return particleStates.reduce(
    (bestSoFar, candidateState) => {
      return selectOptimumEnergyState(bestSoFar, candidateState)
    });
}

function updatePosition(
  particleBestPosition: ParticlePosition, 
  globalBestPosition: ParticlePosition
) {
  const mean = vectorScale(.5, vectorAdd(particleBestPosition, globalBestPosition));
  const standardDeviation = vectorNorm(
    vectorSubtract(particleBestPosition, globalBestPosition)
  );
  return mean.map(
    (coordinate) => sampleNormalDistribution1D(coordinate, standardDeviation)
  );
}

async function evaluatePositions(
  particlePositions: Array<ParticlePosition>, 
  objectiveFunction: ObjectiveFunction,
  numWorkers: number,
  packetSize: number
): Promise<ParticleState[]> {
  const positionPackets = splitToPackets(particlePositions, packetSize);
  const limit = pLimit(numWorkers);
  const functionValues = joinPackets(
    await Promise.all(positionPackets.map(
      (position) => limit(() => objectiveFunction(position))))
    );
  return zipWith(
    particlePositions,
    functionValues,
    (particlePosition, particleEnergy) => ({
      position: particlePosition,
      energy: particleEnergy
    }));
}

function updateStates(
  bestParticleStates: Array<ParticleState>,
  objectiveFunction: ObjectiveFunction,
  numWorkers: number,
  packetSize: number,
): Promise<ParticleState[]> {
  const globalBestState = getBestState(bestParticleStates);
  const updatedPositions = bestParticleStates.map(
    (particleState) => updatePosition(
      particleState.position, globalBestState.position
    ))
  return evaluatePositions(updatedPositions, objectiveFunction, numWorkers, packetSize);
}

function splitToPackets<T>(elements: T[], packetSize: number): T[][] {
  const initial: T[][] = [[]];
  return elements.reduce((packets, position) => {
    const currentPacket = packets.at(-1);
    assert(currentPacket !== undefined);
    if (currentPacket.length === packetSize) {
      packets.push([position]);
    }
    else {
      currentPacket.push(position);
    }
    return packets;
  }, initial)
}

function joinPackets<T>(packets: T[][]): T[] {
  return packets.flat()
}

export async function particleSwarmOptimize(
  initialPositions: Array<ParticlePosition>, 
  objectiveFunction: ObjectiveFunction,
  maxIterations: number,
  numWorkers: number,
  packetSize: number
) {
  let bestStates = await evaluatePositions(
    initialPositions, objectiveFunction, numWorkers, packetSize
  );
  for (let i = 0; i < maxIterations; i++) {
    const candidateStates = await updateStates(
      bestStates, objectiveFunction, numWorkers, packetSize
    );
    bestStates = zipWith(candidateStates, bestStates, selectOptimumEnergyState);
  }
  return getBestState(bestStates);
}
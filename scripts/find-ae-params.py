"""
This script runs a simulation on random byte streams to find the optimal window size (parameter `h`) for the AE chunker.
"""
import random

class AEChunker:
    def __init__(self, window_size):
        self.window_size = window_size
        self.state = self.ChunkerState()

    class ChunkerState:
        def __init__(self):
            self.pos = 0
            self.max_value = -1
            self.max_position = -1

        def reset(self):
            self.pos = 0
            self.max_value = -1
            self.max_position = -1

    def find_boundary(self, data):
        for i, b in enumerate(data):
            global_pos = self.state.pos + i

            if b <= self.state.max_value:
                if global_pos == self.state.max_position + self.window_size:
                    self.state.pos += i + 1
                    return i + 1 
            else:
                self.state.max_value = b
                self.state.max_position = global_pos

        self.state.pos += len(data)
        return None

    def reset(self):
        self.state.reset()

def infinite_random_bytes():
    while True:
        yield random.randint(0, 255)

# Simulate finding boundaries 10000 times
def simulate_chunker(chunker, iterations=10000, block_size=100):
    positions = []

    for _ in range(iterations):
        chunker.reset()
        data_stream = infinite_random_bytes()
        pos = 0
        while True:
            data_block = [next(data_stream) for _ in range(block_size)]
            boundary = chunker.find_boundary(data_block)
            if boundary is not None:
                pos += boundary
                positions.append(pos)
                break
            pos += block_size

    return positions

if __name__ == "__main__":
    for window_size in range(1, 10000):
      chunker = AEChunker(window_size=window_size)
      positions = simulate_chunker(chunker, iterations=10000)
      mean_position = sum(positions) / len(positions)
      print(window_size, mean_position)

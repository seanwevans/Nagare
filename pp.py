#!/usr/bin/env python3

import imageio
import os
import shutil
import sys
import tempfile

import numpy as np
import matplotlib.pyplot as plt


_i = 0


# parse(filepath, (int, float, float), sep=' ')
def parse_file(file_path):
    with open(file_path, 'r') as f:
        for line in f:
            parts = line.strip().split()
            iteration, x, y = int(parts[0]), float(parts[1]), float(parts[2])
            yield iteration, x, y


def generate_frame(x_values, y_values, idx, n, folder):
    fig, ax = plt.subplots(figsize=(5, 5))
    ax.set_xlim(-20, 20)
    ax.set_ylim(-20, 20)
    ax.set_aspect('equal', 'box')
    ax.plot(x_values, y_values, 'o-', alpha=1/100)
    file_path = os.path.join(folder, f"frame_{idx}.png")
    fig.savefig(file_path, dpi=80)
    plt.close(fig)
    return file_path


def compose_gif(file_path, out='animation_from_file.gif', fps=10):
    x_values = []
    y_values = []
    frame_files = []

    temp_folder = tempfile.mkdtemp()    
    with open(file_path, 'r') as f:
        n = sum(1 for _ in f)
    
    interrupted = False

    try:
        for idx, (iteration, x, y) in enumerate(parse_file(file_path)):
            x_values.append(x)
            y_values.append(y)
            frame_file = generate_frame(x_values, y_values, idx, n, temp_folder)
            frame_files.append(frame_file)
            print(f"\rProcessed {idx+1} of {n} lines...", end="")
    except KeyboardInterrupt:
        interrupted = True
        print("\nInterrupt detected. Wrapping up the GIF creation...")

    duration = int(1000 / fps)
    with imageio.get_writer(out, mode='I', duration=duration) as writer:
        for frame_file in frame_files:
            image = imageio.imread(frame_file)
            writer.append_data(image)
    
    shutil.rmtree(temp_folder)

    if interrupted:
        print("\nGIF creation interrupted")
    else:
        print("\nGIF creation complete!")

    return out

def main(args):
    file_path = "out.txt"
    compose_gif(file_path, out='out.gif', fps=20)


if __name__ == "__main__":
    main(sys.argv[1:])

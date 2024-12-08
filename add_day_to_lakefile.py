#!/usr/bin/env nix-shell
#!nix-shell -i python -p python3Packages.toml

import sys
import toml

def add_day_to_lakefile(file_path, day_number):
    try:
        with open(file_path, 'r') as file:
            lakefile = toml.load(file)

        # Update defaultTargets
        default_targets = lakefile.get('defaultTargets', [])
        new_target = f"day{day_number}"
        if new_target in default_targets:
            print(f"Target '{new_target}' already exists in defaultTargets.")
        else:
            default_targets.append(new_target)
            lakefile['defaultTargets'] = default_targets

        # Add new lean_exe entry
        lean_exes = lakefile.get('lean_exe', [])
        new_exe = {
            "name": f"day{day_number}",
            "root": f"Aoc2024.Day{day_number:02d}.Main"
        }
        if any(exe['name'] == new_exe['name'] for exe in lean_exes):
            print(f"Lean exe '{new_exe['name']}' already exists.")
        else:
            lean_exes.append(new_exe)
            lakefile['lean_exe'] = lean_exes

        # Write back to the file
        with open(file_path, 'w') as file:
            toml.dump(lakefile, file)

        print(f"Day {day_number} added successfully.")
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python add_day_to_lakefile.py <lakefile.toml> <N>")
        sys.exit(1)

    file_path = sys.argv[1]
    try:
        day_number = int(sys.argv[2])
        add_day_to_lakefile(file_path, day_number)
    except ValueError:
        print("Error: N must be an integer.")
        sys.exit(1)

#!/usr/bin/env nix-shell
#!nix-shell -i python -p python3Packages.pyyaml

import sys
import yaml

def add_day_to_taskfile(file_path, day_number):
    try:
        with open(file_path, 'r') as file:
            taskfile = yaml.safe_load(file)

        day_task_name = f"day{day_number}"
        if day_task_name in taskfile['tasks']:
            print(f"Task '{day_task_name}' already exists in {file_path}.")
            return

        taskfile['tasks'][day_task_name] = {
            'desc': f"Run day {day_number}",
            'deps': ['build'],
            'cmds': [f".lake/build/bin/day{day_number}"]
        }

        with open(file_path, 'w') as file:
            yaml.dump(taskfile, file, default_flow_style=False, sort_keys=False)

        print(f"Task '{day_task_name}' added successfully.")
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python add_day_to_taskfile.py <Taskfile.yml> <N>")
        sys.exit(1)

    file_path = sys.argv[1]
    try:
        day_number = int(sys.argv[2])
        add_day_to_taskfile(file_path, day_number)
    except ValueError:
        print("Error: N must be an integer.")
        sys.exit(1)

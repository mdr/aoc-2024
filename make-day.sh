#!/bin/bash

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 NN"
    exit 1
fi

day_number=$(printf "%02d" "$1")
source_dir="DayXX"
target_dir="Aoc2024/Day$day_number"

# Ensure the target day is in the correct format
if [[ ! $day_number =~ ^[0-9]{2}$ ]]; then
    echo "Error: target day must be in the format 'NN' where NN is a two-digit number."
    exit 1
fi

# Copy directory to new location
cp -R "$source_dir" "$target_dir"

# Replace XX with NN in file contents
find "$target_dir" -type f -exec sed -i '' "s/XX/${day_number}/g" {} +

# # Rename files and directories containing XX
# find "$target_dir" -depth -name "*XX*" | while read path; do
#     new_path=$(echo "$path" | sed "s/XX/${target_dir:3}/g")
#     mv "$path" "$new_path"
# done

echo "Directory $source_dir successfully copied to $target_dir and updated."

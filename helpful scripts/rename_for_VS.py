import os

# Define the path to the folder
folder_path = '/Users/ritalavi/Desktop/Urmi_fieldwork/data/vs'

# List all files in the folder for debugging
print("Files in directory before renaming:")
for filename in os.listdir(folder_path):
    print(filename)

# Iterate through each file in the folder
for filename in os.listdir(folder_path):
    # Check if "Â" is in the filename
    if "Â" in filename:
        # Create the new filename by replacing "Â" with "AH"
        new_filename = filename.replace("Â", "AH")
        
        # Define the full paths for old and new filenames
        old_file_path = os.path.join(folder_path, filename)
        new_file_path = os.path.join(folder_path, new_filename)

        # Rename the file
        os.rename(old_file_path, new_file_path)
        print(f"Renamed: {filename} to {new_filename}")

print("Renaming completed.")








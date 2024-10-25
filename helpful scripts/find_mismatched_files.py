import os

# Define paths to the folders
wav_folder = "/Users/ritalavi/Desktop/sf"
textgrid_folder = "/Users/ritalavi/Desktop/tg"

# Get list of all .wav and .TextGrid files
wav_files = {os.path.splitext(f)[0] for f in os.listdir(wav_folder) if f.endswith('.wav')}
textgrid_files = {os.path.splitext(f)[0] for f in os.listdir(textgrid_folder) if f.endswith('.TextGrid')}

# Find .wav files with no matching .TextGrid file
wav_mismatches = wav_files - textgrid_files
if wav_mismatches:
    print("No matching TextGrid file for the following .wav files:")
    for file in wav_mismatches:
        print(f"{file}.wav")

# Find .TextGrid files with no matching .wav file
textgrid_mismatches = textgrid_files - wav_files
if textgrid_mismatches:
    print("No matching WAV file for the following .TextGrid files:")
    for file in textgrid_mismatches:
        print(f"{file}.TextGrid")

import os
import shutil
import sys

def copy_newest_file(source_dir, destination_file):
    # Get list of files in source directory
    files = os.listdir(source_dir)
    
    # Filter out directories, system files, hidden files, and script file from the list of files
    files = [f for f in files if os.path.isfile(os.path.join(source_dir, f)) 
             and not f.startswith('.') 
             and not f.lower() == 'desktop.ini'  # Exclude desktop.ini on Windows
             and not f.endswith('~')]  # Exclude backup files ending with '~'
    
    if not files:
        print("No valid files found in the source directory.")
        return
    
    # Get the newest file based on modification time
    newest_file = max(files, key=lambda f: os.path.getmtime(os.path.join(source_dir, f)))
    
    # Construct the destination path
    destination_dir = os.path.dirname(destination_file)
    destination_file_name = os.path.basename(destination_file)
    destination_path = os.path.join(destination_dir, destination_file_name)
    
    # Check if the source and destination paths are the same
    if os.path.abspath(source_dir) == os.path.abspath(destination_dir):
        print("Source directory and destination directory are the same.")
        return
    
    # Create the destination directory if it doesn't exist
    if not os.path.exists(destination_dir):
        os.makedirs(destination_dir)
    
    # Copy the newest file to the destination directory with the desired name
    shutil.copy(os.path.join(source_dir, newest_file), destination_path)
    
    print(f"Newest file '{newest_file}' copied successfully from {source_dir} to {destination_path}")

if __name__ == "__main__":
    # Check if correct number of command-line arguments are provided
    if len(sys.argv) != 3:
        print("Usage: python script.py <source_directory> <destination_file>")
        sys.exit(1)
    
    source_directory = sys.argv[1]
    destination_file = sys.argv[2]

    # Verify if source directory exists
    if not os.path.exists(source_directory):
        print("Source directory does not exist.")
        sys.exit(1)
    
    # Verify if source directory is a directory
    if not os.path.isdir(source_directory):
        print("Source directory is not a valid directory.")
        sys.exit(1)

    copy_newest_file(source_directory, destination_file)

import os
import re
from datetime import datetime

# Define the directory where your Org-Roam files are stored
org_roam_directory = './original'

# Define the converted folder where renamed files will be placed
converted_directory = './converted'

sequential_number = 0

# Create the converted directory if it doesn't exist
if not os.path.exists(converted_directory):
    os.makedirs(converted_directory)

# Function to generate a sequential timestamp (Date + Time + Sequential Number)
def generate_sequential_timestamp():
    global sequential_number
    # Get the current date-time in the required format: YYYYMMDDT
    current_date = datetime.now().strftime("%Y%m%d")
    current_time = datetime.now().strftime("%H%M%S")
    
    # Generate a sequential number and ensure it is always 6 digits long (with leading zeros if needed)
    seq_n = sequential_number
    seq_number = f"{seq_n:06d}"
    sequential_number += 1  # Increment the sequential number for the next file
    
    # Combine date, time, and sequential number
    timestamp = f"{current_date}T{seq_number}"
    return timestamp

# Function to generate the new filename
def generate_new_filename(file_path):
    global sequential_number
    # Get the original filename (without the extension) to use as the title
    original_filename = os.path.splitext(os.path.basename(file_path))[0]
    
    # Replace spaces in the filename with underscores
    title = original_filename.replace(' ', '_')
    
    # Get a sequential timestamp for the file's unique identifier
    date_identifier = generate_sequential_timestamp()
    
    # Extract keywords from the tags (assuming the tags are formatted like: :keyword1:keyword2:)
    with open(file_path, 'r') as file:
        content = file.read()
        
        # Extract keywords from the tags (if any)
        keywords_match = re.search(r':(.*):', content)
        keywords = keywords_match.group(1) if keywords_match else ''
        
        # Clean up keywords, replace spaces with underscores
        keywords = keywords.replace(' ', '_') if keywords else ''
        
        # Combine the components to create the new filename
        extension = ".org"  # Modify if you use a different extension
        new_filename = f"{date_identifier}--{title}__{keywords}{extension}"
    
    # Get the full path of the new filename in the converted folder
    new_file_path = os.path.join(converted_directory, new_filename)
    
    return new_filename, new_file_path, date_identifier  # Also return the timestamp for links


# Function to update Org-Roam ID links after renaming
def update_org_roam_links(file_path):
    # Skip lock files (those starting with .#)
    if file_path.startswith('./converted/.#'):
        return

    with open(file_path, 'r') as file:
        content = file.read()

    # Regex pattern to find Org-Roam ID links: [[id:org-roam-id][description]]
    link_pattern = r'\[\[id:(.*?)\]\[(.*?)\]\]'
     
    # Replace the old file ID links with the new file link
    def replace_link(match):
        old_id = match.group(1)
        description = match.group(2)
        print('file :', file_path, 'has link id:', old_id, 'description: ', description)

        # Find the file that matches the old ID (search by filename in the converted folder)
        for root, dirs, files in os.walk(converted_directory):
            for file in files:
                if file.endswith(".org"):
                    #if file has the id in the properties
                    if ':ID: '+old_id in open(os.path.join(root, file)).read():
                        new_link = f"[[file:{file}][{description}]]"
                        return new_link

        # If no match is found, return the original link (though ideally this shouldn't happen)
        return match.group(0)

    # Replace all occurrences of old Org-Roam ID links with the updated ones
    new_content = re.sub(link_pattern, replace_link, content)

    # Write the updated content back to the file
    with open(file_path, 'w') as file:
        file.write(new_content)

# Step 1: Convert and Rename All Files
for root, dirs, files in os.walk(org_roam_directory):
    for file in files:
        if file.endswith(".org"):
            original_file_path = os.path.join(root, file)
            
            # Generate the new file name and its full path in the converted folder
            new_filename, new_file_path, _ = generate_new_filename(original_file_path)
            
            # Copy the original content into the new file in the converted folder
            with open(original_file_path, 'r') as src_file:
                content = src_file.read()
            with open(new_file_path, 'w') as dest_file:
                dest_file.write(content)

            print(f'File copied and renamed: {original_file_path} -> {new_file_path}')

# Step 2: Update Links in the Renamed Files
for root, dirs, files in os.walk(converted_directory):
    for file in files:
        if file.endswith(".org"):
            file_path = os.path.join(root, file)
            update_org_roam_links(file_path)

            print(f'Links updated in file: {file_path}')


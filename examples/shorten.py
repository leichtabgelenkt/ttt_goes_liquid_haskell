import re
import random
import subprocess
import os

def parse_trs_file(filename):
    with open(filename, 'r') as file:
        content = file.read()
    return content

def generate_unique_name(existing_names, all_vars):
    """Generate a unique single-letter name that doesn't conflict with existing names or variables."""
    alphabet = 'abcdefghijklmnopqrstuvwxyz'
    
    # Try to create a unique name by iterating through the alphabet
    for char in alphabet:
        if char not in existing_names and char not in all_vars:
            return char
    raise ValueError("Not enough unique names available!")

def rename_functions_and_constants_in_trs(trs_content):
    # Regular expressions to match function names, constants, and variables
    function_pattern = r'([a-zA-Z_]\w*)\('  # Matches function names followed by '('
    constant_pattern = r'\b(true|false|null|nil)\b'  # Matches constants like true, false, nil
    variable_pattern = r'\b([A-Z])\b'  # Matches single-letter variables (e.g., M, N, X, etc.)

    # Find all function names, constants, and variables in the TRS content
    rules_start = trs_content.find("(RULES") + len("(RULES")  # Find the start of rules
    rules_content = trs_content[rules_start:]

    # Extract function names, constants, and variables
    functions_in_rules = set(re.findall(function_pattern, rules_content))
    constants_in_trs = set(re.findall(constant_pattern, trs_content))
    variables_in_trs = set(re.findall(variable_pattern, trs_content))

    # New names for functions and constants
    new_function_names = {}
    new_constant_names = {}
    unique_function_names = set()
    unique_constant_names = set()

    # Collect all used names (functions, constants, and variables)
    used_names = variables_in_trs.union(functions_in_rules).union(constants_in_trs)

    # Process functions (rename if longer than 1 character)
    for func in functions_in_rules:
        if len(func) > 1 and func not in variables_in_trs:  # Function name is longer than 1 char
            if func not in new_function_names:
                # Generate a unique name for the function
                new_name = generate_unique_name(used_names, variables_in_trs)
                new_function_names[func] = new_name
                used_names.add(new_name)

    # Process constants (rename them)
    for const in constants_in_trs:
        if const not in new_constant_names:
            # Generate a unique name for the constant
            new_name = generate_unique_name(used_names, variables_in_trs)
            new_constant_names[const] = new_name
            used_names.add(new_name)

    # Replace function names and constants in the rules content
    def replace_function(match):
        func_name = match.group(1)
        if func_name in new_function_names:
            return f"{new_function_names[func_name]}("
        return match.group(0)

    def replace_constant(match):
        const_name = match.group(1)
        if const_name in new_constant_names:
            return f"{new_constant_names[const_name]}"
        return match.group(0)

    updated_trs_content = re.sub(function_pattern, replace_function, trs_content)
    updated_trs_content = re.sub(constant_pattern, replace_constant, updated_trs_content)

    return updated_trs_content

def save_trs_file(filename, content):
    with open(filename, 'w') as file:
        file.write(content)

# Main function to handle the renaming process
def process_trs_file(input_filename, output_filename):
    trs_content = parse_trs_file(input_filename)
    updated_trs_content = rename_functions_and_constants_in_trs(trs_content)
    save_trs_file(output_filename, updated_trs_content)
    print(f"Updated TRS content saved to {output_filename}")

#folders = ['./examples/Rubio_04/trs_files/', './examples/Secret_05_TRS/trs_files/', './examples/Secret_06_TRS/trs_files/', 
#          './examples/Secret_07_TRS/trs_files/', './examples/Various_04/trs_files/', './examples/Yamada_21/trs_files/'] #'./examples/HirokawaMiddeldorp_04/trs_files/', 

folders = ['./examples/Der95/trs_files/', './examples/SK90/trs_files/','./examples/payet_21/trs_files/', './examples/Payet_23/trs_files/',
            './examples/TCT_12/trs_files/', './examples/Waldmann_06/trs_files/', './examples/Rubio_04/trs_files/', './examples/Secret_05_TRS/trs_files/', './examples/Secret_06_TRS/trs_files/', 
            './examples/Secret_07_TRS/trs_files/', './examples/Various_04/trs_files/', './examples/Yamada_21/trs_files/', './examples/GTSSK07/trs_files/',
            './examples/Waldmann_23/trs_files/', './examples/Zantema_05/trs_files/', './examples/Zantema_15/trs_files/', './examples/Mixed_TRS/trs_files/',
            './examples/Hydras/trs_files/', './examples/MNZ_10/trs_files/', './examples/Endrullis_06/trs_files/']

for folder in folders:
    for file_name in os.listdir(folder):
        if file_name.endswith(".trs"):
            # Example usage
            input_filename = folder + file_name
            output_filename = folder + "short_" + file_name
            process_trs_file(input_filename, output_filename)

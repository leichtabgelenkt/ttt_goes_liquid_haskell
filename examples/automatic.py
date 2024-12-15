import random
import subprocess
import os

# Datei einlesen und in einen String umwandeln
def trs_to_string(file_path):
    with open(file_path, 'r') as file:
        trs_content = file.read()
    return trs_content

def weighted_random_key(my_dict):
    # Assign weights: larger weight for value 0
    weights = [13 if value == 0 else 1 for value in my_dict.values()]

    # Select a random key based on the weights
    selected_key = random.choices(list(my_dict.keys()), weights=weights, k=1)[0]
    return selected_key

def get_terms(lines):
    sides = []
    for line in lines:
        temp = line.split("->")
        sides.append(temp[0].replace(" ", ""))
        sides.append(temp[1].replace(" ", ""))
    return sides

def get_arities(sides):
    arity_map = {}
    for side in sides:
        for i in range(0, len(side)):
            keys = arity_map.keys()
            current = side[i]
            if(current in keys or current in "()" or current in variables or current == ','):
                continue
            else:
                brace_count = 0
                arity = 1
                for j in range(i+1, len(side)):
                    if(side[i+1] == ")"):
                        arity_map[side[i]] = 0
                        break
                    if(side[j] == "("):
                        brace_count -= 1
                    elif(side[j] == ")"):
                        brace_count += 1
                    elif(side[j] == "," and brace_count == -1):
                        arity += 1
                    
                    if(brace_count == 0):
                        arity_map[side[i]] = arity
                        arity = 0
                        break
    
    return arity_map

def get_component_from_key():
    pulled_key = weighted_random_key(arities)
    subterm = ''
    for component in term_components:
        if pulled_key in component:
            subterm = component
            break
    
    return subterm

    

def create_random_start_terms(n):
    start_terms = set()
    while(len(start_terms) < n):
        start = get_component_from_key()
        while('C' in start):
            start = start.replace('C', get_component_from_key(), 1)

        start_terms.add(start)

    return(start_terms)



def get_term_components(arities):
    components = []
    for key in arities.keys():
        if(arities[key] == 0):
            components.append(key)
            continue
        temp = key + "(C"
        for i in range(0, arities[key]-1):
            temp += ",C"
        temp += ")"
        components.append(temp)

    return components

total = 0
success = 0
folders = ['./examples/Der95/trs_files/', './examples/SK90/trs_files/']

for folder in folders:
    for file_name in os.listdir(folder):
        if file_name.endswith(".trs"):
            # Beispielaufruf
            file_path = folder + file_name  # Ersetze durch den tatsächlichen Pfad zur Datei
            trs_string = trs_to_string(file_path)
            print(file_path)

            parts = trs_string.split("\n")
            variables = parts[0].removeprefix("(VAR ").removesuffix(")").split(" ")
            sides = get_terms(parts[3:-2])
            arities = get_arities(sides)
            if all(value != 0 for value in arities.values()):
                print("There are no constants in the siganture, so we can't build ground terms. Added constant B")
                arities['B'] = 0
            term_components = get_term_components(arities)
            start_terms = create_random_start_terms(5)

            # Base command and initial arguments
            base_command = ["stack", "run", "subterm-criterion"]

            output_file_path = file_path.replace(".trs", "_result.txt")

            # Open the file in write mode
            with open(output_file_path, "w") as output_file:
                for term in start_terms:
                    total += 1
                    # Combine the base command with the dynamically added arguments
                    full_command = base_command + [file_path, term]

                    # Execute the command
                    result = subprocess.run(full_command, capture_output=True, text=True)
                    if("Success!!" in result.stdout):
                        success += 1
                    output_file.write(f"{result.stdout}\n")
                    output_file.write("=" * 40 + "\n")  # Add a separator for readabilit

print(f"From {total} startterms {success} were proven to terminate for their TRS. This makes a {(success / total)*100}% sucess rate!")
                    

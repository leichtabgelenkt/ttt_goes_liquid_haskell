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
    weights = [constant_probability if value == 0 else 1 for value in my_dict.values()]

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
timeout_time = 30
timeouts_subterm_criterion = 0
timeouts_rest = 0
n_start_terms = 25
constant_probability = 8
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
            sides = get_terms(parts[2:-3])
            arities = get_arities(sides)
            if all(value != 0 for value in arities.values()):
                print("There are no constants in the siganture, so we can't build ground terms. Added constant B")
                arities['B'] = 0
            term_components = get_term_components(arities)
            start_terms = create_random_start_terms(n_start_terms)
            total += n_start_terms

            # Base command and initial arguments
            base_commands = [["stack", "run", "subterm-criterion"], ["stack", "run", "rest"]] ##["stack", "run", "subterm-criterion"], 
            i = 0

            for base_command in base_commands:
                output_file = ""
                if("subterm-criterion" in base_command):
                    output_file_path = file_path.replace(".trs", "_result_subterm-criterion.txt")
                else:
                    output_file_path = file_path.replace(".trs", "_result_rest.txt")

                # Open the file in write mode
                with open(output_file_path, "w") as output_file:
                    for term in start_terms:
                        # Combine the base command with the dynamically added arguments
                        full_command = base_command + [file_path, term]

                        try:
                            # Running the subprocess with a timeout of 30 seconds
                            result = subprocess.run(full_command, capture_output=True, text=True, timeout=timeout_time)
                            
                            if( "Success!!" in result.stdout):
                                success += 1

                            
                            output_file.write(f"{result.stdout}\n")
                            output_file.write("=" * 40 + "\n")  # Add a separator for readability
                        
                        except subprocess.TimeoutExpired:
                            if(i == 0):
                                timeouts_subterm_criterion += 1
                            else:
                                timeouts_rest += 1
                            output_file.write(f"The subterm criterion timed out after {timeout_time} seconds, for the start term {term}.\n")
                            output_file.write("=" * 40 + "\n")  # Add a separator for readability
                
                i += 1

print("Subterm-criterion:")
print(f"From {total} startterms {success} were proven to terminate for their TRS, whereas {timeouts_subterm_criterion} terms timed out (Could have been successes). This makes a {(success / total)*100}% sucess rate!")

print("\n\n\n#####################################\n\n\n")
print("rest:")
print(f"From {total} startterms rest timed-out {timeouts_rest} times. This makes a {(timeouts_rest/ total)*100}% time out rate!")
                    

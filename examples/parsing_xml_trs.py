import xml.etree.ElementTree as ET
from pathlib import Path
from collections import defaultdict
import re
import sys

mod_path = Path(__file__).parent
relative_path = "" 
relative_path2 = ""
xml_file = ""
output_file = ""



# Define maps for rewrite rules and function signatures
rewrite_rules = []
function_signatures = {}
old_new_functions = {}
variables = set()

# Parse the XML file
def parse_xml(file_path):
    tree = ET.parse(file_path)
    root = tree.getroot()

    find_vars(file_path)

    # Extract function signatures
    signature = root.find("trs/signature")
    func_counter = 0
    funcs_ordered_with_name_length = sorted([func.find("name").text.strip() for func in signature.findall("funcsym")], key=len, reverse=True)
    funcs_ordered = sorted(signature.findall("funcsym"), key=lambda func: len(func.find("name").text.strip()), reverse=True)
    for func in funcs_ordered:
        
        name = func.find("name").text.strip()
        new_name = chr(ord("a")+ func_counter)
        
        while new_name in old_new_functions.values() or new_name in variables or new_name in funcs_ordered_with_name_length: #or new_name in [key[0].lower() for key in old_new_functions.keys()]
            new_name = chr(ord(new_name) + 1)

        old_new_functions[name] = new_name

        arity = int(func.find("arity").text.strip())
        function_signatures[new_name] = arity
        func_counter += 1
    
    # Extract rewrite rules
    rules = root.find("trs/rules")
    for rule in rules.findall("rule"):
        lhs = ''.join(rule.find("lhs").itertext()).strip().replace('\n', '')
        rhs = ''.join(rule.find("rhs").itertext()).strip().replace('\n', '')

        for name in old_new_functions.keys():
            lhs = lhs.replace(name, old_new_functions[name])
            rhs = rhs.replace(name, old_new_functions[name])

        rewrite_rules.append({"lhs": lhs, "rhs": rhs})


# Function to print all information
def print_trs_info():
    print("Rewrite Rules:")
    for idx, rule in enumerate(rewrite_rules, 1):
        print(f"  Rule {idx}: {rule['lhs']} -> {rule['rhs']}")

    print("\nFunction Signatures:")
    for func, arity in function_signatures.items():
        print(f"  {func}: arity {arity}")

def build_side_string_rec(bold_hs):
    return_str = ""
    if bold_hs[0] in function_signatures.keys():
        # when the function is a constant
        
            
        if function_signatures[bold_hs[0]] == 0:
            return_str = return_str + bold_hs[0]
        else:
            return_str = return_str + bold_hs[0] + "("
            # for all arities call the function new
            for i in range(1, function_signatures[bold_hs[0]] + 1):
                return_str = return_str + build_side_string_rec(bold_hs[i:])
                if i < function_signatures[bold_hs[0]]:
                    return_str = return_str + ","
                else:
                    return_str = return_str + ")"
    elif bold_hs[0] in variables:
        return_str = return_str + bold_hs[0]

    return return_str

def make_rule_string(rule):
    rule_string = ""
    bold_lhs = rule['lhs']
    lhs = build_side_string_rec(bold_lhs)
    # print(lhs)
    # lhs = bold_lhs

    bold_rhs = rule['rhs']
    rhs = build_side_string_rec(bold_rhs)
    # print(lhs)
    rule_string = (f"{' ' * 4}{lhs} -> {rhs}\n")
    return rule_string

# Function to write TRS to a file
def write_trs_to_file(output_path):
    with open(output_path, 'w') as f:
        f.write("(VAR " + " ".join(sorted(variables)) + ")\n")

        # Write rules
        f.write("(RULES\n")
        for rule in rewrite_rules:
            f.write(make_rule_string(rule))
        f.write("\n")
        f.write(")\n")

def find_vars(file):
    with open(file, 'r') as file:
        xml_content = file.read()
        # Regular expression to find content inside <var>...</var>
        pattern = r"<var>(.*?)</var>"
        # Find all matches
        variables.update(re.findall(pattern, xml_content))
        # print(variables)


if __name__ == "__main__":
    try:
        # relative_path = "./SK90/4.28.xml"
        # relative_path2 = "./SK90/4.28.trs"

        if len(sys.argv) != 3:
            print("Usage: python3 parsing_xml_trs.py <input_xml_file> <output_trs_file>")
            sys.exit(1)
        relative_path = sys.argv[1]
        relative_path2 = sys.argv[2]

        xml_file = (mod_path / relative_path).resolve()
        output_file = (mod_path / relative_path2).resolve()

        parse_xml(xml_file)
        write_trs_to_file(output_file)
    except Exception as e:
        print(f"An error occurred: {e}")
        sys.exit(1)



def use_as_library(xml_file, output_file):
    try:
        parse_xml(xml_file)
        write_trs_to_file(output_file)
    except Exception as e:
        print(f"An error occurred: {e}")
        sys.exit(1)
    return output_file
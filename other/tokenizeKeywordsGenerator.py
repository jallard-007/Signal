import sys

class Keyword:
    def __init__(self, token_type: str, value: str) -> None:
        self.token_type = token_type
        self.value = value
    
    def __str__(self) -> str:
        return self.value + ": " + self.token_type

def default() -> None:
    pass

def strncmp() -> None:
    keywords_file = sys.argv[1]
    with open(keywords_file, "r") as file:
        keywords = [Keyword(line.split("|")[0].strip(), line.split("|")[1].strip()) for line in file.readlines()]
    
    keywords.sort(key = lambda this: this.value)

    cases: dict[str, str] = {}

    for keyword in keywords:
        char = keyword.value[0]
        if keyword.value[0] not in cases:
            cases[char] = f"case '{char}': {{\n"
            cases[char] += f"    if (strncmp((const char *)&content[++position], \"{keyword.value[1:]}\", {len(keyword.value) - 1}) == 0) {{\n"
        else:
            cases[char] += f"    else if (strncmp((const char *)&content[position], \"{keyword.value[1:]}\", {len(keyword.value) - 1}) == 0) {{\n"
        cases[char] += f"         position += {len(keyword.value) - 1};\n"
        cases[char] += f"         END_OF_IDENTIFIER({keyword.token_type})\n"
        cases[char] += f"    }}\n"

    for char in cases:
        cases[char] += "    movePastIdentifier(); break;\n}"
        print(cases[char])


default()

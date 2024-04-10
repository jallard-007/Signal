import sys

class Keyword:
    def __init__(self, token_type: str, value: str) -> None:
        self.token_type = token_type
        self.value = value
    
    def __str__(self) -> str:
        return self.value + ": " + self.token_type

def new_method() -> None:
    keywords_file = sys.argv[1]
    with open(keywords_file, "r") as file:
        keywords = [Keyword(line.split("|")[0].strip(), line.split("|")[1].strip()) for line in file.readlines()]
    
    keywords.sort(key = lambda this: this.value)

    length_to_char_to_keywords: dict[int, dict[str, list[Keyword]]] = {}

    for keyword in keywords:
        length = len(keyword.value)
        if length not in length_to_char_to_keywords:
            length_to_char_to_keywords[length] = {}
        char = keyword.value[0]
        if char not in length_to_char_to_keywords[length]:
            length_to_char_to_keywords[length][char] = []
        assert keyword not in length_to_char_to_keywords[length][char]
        length_to_char_to_keywords[length][char].append(keyword)


    indentation = "    "
    base_indentation = 3
    length_cases: dict[int, str] = {}
    for length in length_to_char_to_keywords:
        length_cases[length] = f"{indentation*(base_indentation + 1)}case {length}: {{\n"
        length_cases[length] += f"{indentation*(base_indentation + 2)}switch (c) {{\n"
        length_to_char_to_keywords[length] = dict(sorted(length_to_char_to_keywords[length].items()))
        for char in length_to_char_to_keywords[length]:
            length_cases[length] += f"{indentation*(base_indentation + 3)}case '{char}': {{\n"
            for keyword in length_to_char_to_keywords[length][char]:
                substr = keyword.value[1:]
                length_cases[length] += f"{indentation*(base_indentation + 4)}if (strncmp((const char *)&content[tokenStartPos +1], \"{substr}\", sizeof(\"{substr}\") - 1) == 0) {{\n"
                length_cases[length] += f"{indentation*(base_indentation + 5)}type = {keyword.token_type}; break;\n"
                length_cases[length] += f"{indentation*(base_indentation + 4)}}}\n"
            length_cases[length] += f"{indentation*(base_indentation + 4)}break;\n"
            length_cases[length] += f"{indentation*(base_indentation + 3)}}}\n"
        length_cases[length] += f"{indentation*(base_indentation + 3)}default: break;\n"
        length_cases[length] += f"{indentation*(base_indentation + 2)}}}\n"
        length_cases[length] += f"{indentation*(base_indentation + 2)}break;\n"
        length_cases[length] += f"{indentation*(base_indentation + 1)}}}"

    length_cases = dict(sorted(length_cases.items()))

    setup = f""" 
{indentation*(base_indentation)}movePastIdentifier();
{indentation*(base_indentation)}const uint32_t length = position - tokenStartPos;
{indentation*(base_indentation)}switch (length) {{"""
    print(setup)
    for length in length_cases:
        print(length_cases[length])
    print(f"{indentation*(base_indentation + 1)}default: break;")
    print(f"{indentation*(base_indentation)}}}")

new_method()

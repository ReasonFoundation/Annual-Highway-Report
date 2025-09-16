import fitz  # PyMuPDF
import re
import json
from collections import defaultdict
valid_states = {
    'Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut',
    'Delaware', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa',
    'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan',
    'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire',
    'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio',
    'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota',
    'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia',
    'Wisconsin', 'Wyoming'
}
# Load the PDF
pdf_path = "28th-annual-highway-report-state-by-state-summaries.pdf"


# Regex patterns
#"Compared to neighboring" 49 of this. One of this: "Compared to other somewhat similar" for Alaska case
neighbor_pattern = re.compile(
    r"(Compared to (neighboring and nearby states|other somewhat similar states).*?)(?=\n{2,}|Comparing its overall performance|[A-Z][a-z]+['’]s highway system ranks)",
    re.IGNORECASE | re.DOTALL
)

# "Comparing its overall performance to similarly populated states" found 50 of this in pdf file
similar_alt_pattern = re.compile(
    r"(Comparing its overall performance to similarly populated states.*?)(?=\n\n|$)",
    re.IGNORECASE | re.DOTALL
)

# Result dictionary
result = {}

# Helper to normalize names
def clean_state_name(state):
    name = state.strip()
    name = re.sub(r"^(than|and|of|both|either|but|behind|above|below)\s+", "", name, flags=re.IGNORECASE)
    name = re.sub(r"[’']s$", "", name)
    name = name.strip("'’s ").strip()

    for state in valid_states:
        if state.startswith(name):
            return state
    return name

with fitz.open(pdf_path) as doc:
    # Parse PDF pages
    for page in doc:
        text = page.get_text()

        print(f"\n--- Page {page.number + 1} ---")
        matches_found = False

        # Neighboring states
        for match in neighbor_pattern.finditer(text):
            paragraph = match.group(1)
            print("Matched Neighbor:", paragraph)

            # Try to extract the state name
            state_search = re.search(r"Compared to (?:neighboring|other somewhat similar).*?(\b[A-Z][a-z]+(?: [A-Z][a-z]+)?)['’]s overall", paragraph)
            if state_search:
                state = clean_state_name(state_search.group(1))
            else:
                continue  # Skip if no state identified

            # Extract neighboring state names
            #neighbors_raw = re.findall(r"\b((?:North|South|West|New)? ?[A-Z][a-z]+)['’]s", paragraph)
            neighbors_raw = re.findall(r"\b([A-Z][a-z]+(?: [A-Z][a-z]+)?)['’]s", paragraph)
            additional_neighbors = re.findall(r"than ([A-Z][a-z]+(?: [A-Z][a-z]+)?)|and ([A-Z][a-z]+(?: [A-Z][a-z]+)?)", paragraph)
            additional_neighbors_flat = [name for pair in additional_neighbors for name in pair if name]

            all_neighbors = list(set(neighbors_raw + additional_neighbors_flat))

            if state not in result:
                result[state] = {"neighboring_states": [], "similarly_populated_states": []}

            result[state]["neighboring_states"] = [clean_state_name(s) for s in all_neighbors if clean_state_name(s) != state]

        # Similarly populated states
        for match in similar_alt_pattern.finditer(text):
            paragraph = match.group(1)
            print("Matched Similar:", paragraph)
            state_search = re.search(r"Comparing.*?(\w+(?: \w+)*) ranks", paragraph)
            if state_search:
                state = clean_state_name(state_search.group(1))
                similars = re.findall(r"(\w+(?: \w+)?)\s+\((\d+(?:st|nd|rd|th))\)", paragraph)
                if state not in result:
                    result[state] = {"neighboring_states": [], "similarly_populated_states": []}
                result[state]["similarly_populated_states"] = [clean_state_name(s) for s, _ in similars]


        if not matches_found:
            print("No matches found on this page.")


# Save to JSON
output_path = "state_comparisons_clean.json"
with open(output_path, "w") as f:
    json.dump(result, f, indent=2)

print(f"Saved to {output_path}")


flagged_states = set()

for state, data in result.items():
    if state not in valid_states:
        flagged_states.add(state)
    for neighbor in data['neighboring_states']:
        if neighbor not in valid_states:
            flagged_states.add(neighbor)
    for similar in data['similarly_populated_states']:
        if similar not in valid_states:
            flagged_states.add(similar)

if flagged_states:
    print("⚠️ Flagged non-standard state names:")
    for name in sorted(flagged_states):
        print("-", name)
else:
    print("✅ All state names are valid.")
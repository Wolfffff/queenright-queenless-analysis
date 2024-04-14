import pandas as pd
from typing import Dict, List, Union


def process_tags_to_list_and_queen_per_group(
    file_location: str,
) -> Dict[str, Union[Dict[str, List[int]], Dict[str, int]]]:
    """
    Process tags to list and queen per group.

    Args:
        file_location (str): The location of the CSV file to process.

    Returns:
        dict: A dictionary with two keys: 'tags' and 'queens'.
              'tags' maps from group names to lists of tag IDs.
              'queens' maps from group names to queen tag IDs.
    """
    df = pd.read_csv(file_location, dtype={"Tags": int})
    df = df.fillna("0")
    group_lists = {}
    group_queens = {}

    for i in range(len(df)):
        tag_id = df.loc[i, "Tags"]

        for j in range(1, len(df.columns)):
            column_name = df.columns[j]
            cell_value = df.loc[i, column_name]

            if cell_value == "1" or cell_value.lower() == "queen":
                if column_name not in group_lists:
                    group_lists[column_name] = []
                group_lists[column_name].append(tag_id)

            if cell_value.lower() == "queen":
                group_queens[column_name] = tag_id

    simplified_group_lists = {k: list(set(v)) for k, v in group_lists.items()}

    return {"tags": simplified_group_lists, "queens": group_queens}

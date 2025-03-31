# Anonymization Smart Contract

## Overview
The **Anonymization Smart Contract** is designed to facilitate secure and private data submission for research purposes. It utilizes **ring signatures** to anonymize data submissions while ensuring aggregation and verification of the collected information.

## Features
- **Ring Signature-Based Anonymization:** Ensures privacy by allowing users to submit data without revealing their identity.
- **Data Aggregation:** Collects and processes submitted data for research analysis.
- **Role-Based Access Control:** Limits submission and research access to authorized users.
- **Secure Researcher Management:** Only the contract owner can add or remove authorized researchers.
- **Tamper-Resistant Data Storage:** Stores aggregated data securely on-chain.

## Error Codes
| Code | Description |
|------|-------------|
| `ERR-NOT-AUTHORIZED (u100)` | User is not authorized to perform the action. |
| `ERR-INVALID-DATA (u101)` | Submitted data is invalid. |
| `ERR-RING-SIZE-INVALID (u102)` | Provided ring size is invalid. |

## Data Structures
### Mappings
- **`aggregated-data`**: Stores aggregated statistics for each category.
- **`ring-signatures`**: Stores ring signatures for verification.
- **`authorized-researchers`**: Tracks authorized researchers.

### Variables
- **`submission-counter`**: Tracks the number of submissions.
- **`contract-owner`**: Stores the contract owner.

## Functions
### Public Functions
#### `initialize-contract(researcher: principal) -> (ok true | err)`
Initializes the contract and authorizes a researcher (only callable by the contract owner).

#### `submit-anonymous-data(category: string, value: uint, ring-size: uint, ring-signature: buff) -> (ok submission-id | err)`
Submits anonymized data under a specified category using a ring signature.

#### `add-researcher(researcher: principal) -> (ok true | err)`
Adds a new researcher to the authorized list (only callable by the contract owner).

#### `remove-researcher(researcher: principal) -> (ok true | err)`
Removes a researcher from the authorized list (only callable by the contract owner).

### Read-Only Functions
#### `get-aggregated-data(category: string) -> (ok data | none)`
Retrieves aggregated data for a given category.

### Private Functions
#### `generate-ring-members(size: uint) -> list`
Generates a list of ring members for anonymity.

#### `is-authorized(user: principal) -> bool`
Checks if a user is authorized to submit data.

## Usage
1. **Initialize the contract** by adding an initial authorized researcher.
2. **Submit anonymized data** under different research categories.
3. **Retrieve aggregated data** for research analysis.
4. **Manage researchers** to control data submission access.

## Security Considerations
- Only authorized researchers can submit and retrieve data.
- Ring signatures help maintain anonymity while ensuring verifiable submissions.
- Contract owner has exclusive control over researcher management.

## License
This project is open-source and available for modification under an appropriate open-source license.


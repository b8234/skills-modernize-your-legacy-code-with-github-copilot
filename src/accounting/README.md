# Node.js Account Management System

## Overview

This is a modernized Node.js version of the legacy COBOL Account Management System. The application preserves all original business logic, data integrity, and user interface behavior while providing a modern JavaScript implementation.

## Features

- **Identical Business Logic**: Preserves all original COBOL business rules and transaction behavior
- **Data Integrity**: Maintains precise decimal handling and balance calculations
- **Menu-Driven Interface**: Exact replica of original menu system and user flow
- **Insufficient Funds Protection**: Prevents overdrafts with same validation logic
- **Session Persistence**: In-memory balance storage during application session

## Installation

```bash
cd src/accounting
npm install
```

## Running the Application

### Method 1: Using npm script

```bash
npm start
```

### Method 2: Direct node execution

```bash
node index.js
```

### Method 3: VS Code Debug/Run

- Open VS Code
- Go to Run and Debug (Ctrl+Shift+D)
- Select "Run Node.js Accounting App" or "Debug Node.js Accounting App"
- Press F5 or click the play button

## Application Structure

The Node.js application follows the same modular architecture as the original COBOL system:

```text
Node.js Application Structure:
├── MainProgram Class (main.cob equivalent)
│   ├── Menu display and navigation
│   ├── User input handling
│   └── Program flow control
├── Operations Class (operations.cob equivalent)
│   ├── Balance inquiry (TOTAL)
│   ├── Credit transactions (CREDIT)
│   └── Debit transactions (DEBIT)
└── DataProgram Class (data.cob equivalent)
    ├── Balance storage and retrieval
    ├── Data formatting
    └── Persistence management
```

## Business Rules Preserved

### Account Initialization

- **Starting Balance**: $1,000.00 (same as COBOL default)
- **Currency Format**: Displays as "001000.00" (6 digits + 2 decimals)

### Credit Operations

- No upper limit restrictions
- Accepts any positive amount
- Immediate balance update and confirmation

### Debit Operations

- **Insufficient Funds Protection**: Prevents negative balances
- Only processes when `current_balance >= debit_amount`
- Displays error message for insufficient funds

### Menu Navigation

- Options 1-4 exactly match original system
- Input validation with error messages
- Continuous loop until exit selection

## Menu Options

1. **View Balance** - Displays current account balance
2. **Credit Account** - Add funds to the account
3. **Debit Account** - Withdraw funds (with overdraft protection)
4. **Exit** - Terminate the application

## Dependencies

- **readline-sync**: Provides synchronous console input (replaces COBOL ACCEPT statements)
- **Node.js**: Runtime environment (requires v14.0.0 or higher)

## Error Handling

The application includes enhanced error handling beyond the original COBOL version:

- Input validation for numeric amounts
- Graceful handling of invalid menu selections
- Protection against non-numeric inputs for transaction amounts

## Testing

The application can be tested using the test plan documented in `docs/TESTPLAN.md`. All test cases from the COBOL version apply directly to this Node.js implementation.

## Conversion Notes

### COBOL to Node.js Mappings

| COBOL Concept | Node.js Equivalent |
|---------------|-------------------|
| `CALL 'Program' USING params` | `object.method(params)` |
| `ACCEPT variable` | `readlineSync.question()` |
| `DISPLAY message` | `console.log()` |
| `PIC 9(6)V99` | `number` with `.toFixed(2)` formatting |
| `PERFORM UNTIL condition` | `while (condition)` loop |
| `EVALUATE expression` | `switch (expression)` statement |
| `MOVE value TO variable` | `variable = value` |
| `GOBACK` | `return` statement |

### Enhanced Features

While preserving original behavior, the Node.js version includes:

- Better error messages for invalid inputs
- Graceful handling of non-numeric entries
- Modern JavaScript class structure for maintainability
- Comprehensive documentation and comments
- VS Code debugging support

## Future Enhancements

This foundation supports easy addition of:

- REST API endpoints
- Database persistence
- User authentication
- Transaction history
- Multi-user support
- Web interface
- Automated testing suite

## License

ISC

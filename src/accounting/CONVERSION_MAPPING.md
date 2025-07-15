# COBOL to Node.js Conversion Mapping

## Overview

This document provides a detailed mapping between the original COBOL Account Management System and its Node.js equivalent, demonstrating how each COBOL construct was preserved in the modern implementation.

## File Structure Mapping

| COBOL Files | Node.js Equivalent | Purpose |
|-------------|-------------------|---------|
| `main.cob` | `MainProgram` class | Entry point and user interface controller |
| `operations.cob` | `Operations` class | Business logic for transactions |
| `data.cob` | `DataProgram` class | Data persistence and storage |

## Code Conversion Examples

### 1. Main Program Structure

#### COBOL (main.cob)

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. MainProgram.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  USER-CHOICE       PIC 9 VALUE 0.
01  CONTINUE-FLAG     PIC X(3) VALUE 'YES'.

PROCEDURE DIVISION.
MAIN-LOGIC.
    PERFORM UNTIL CONTINUE-FLAG = 'NO'
        DISPLAY "--------------------------------"
        DISPLAY "Account Management System"
        DISPLAY "1. View Balance"
        DISPLAY "2. Credit Account"
        DISPLAY "3. Debit Account"
        DISPLAY "4. Exit"
        DISPLAY "--------------------------------"
        DISPLAY "Enter your choice (1-4): "
        ACCEPT USER-CHOICE
        
        EVALUATE USER-CHOICE
            WHEN 1
                CALL 'Operations' USING 'TOTAL '
            WHEN 2
                CALL 'Operations' USING 'CREDIT'
            WHEN 3
                CALL 'Operations' USING 'DEBIT '
            WHEN 4
                MOVE 'NO' TO CONTINUE-FLAG
            WHEN OTHER
                DISPLAY "Invalid choice, please select 1-4."
        END-EVALUATE
    END-PERFORM
    DISPLAY "Exiting the program. Goodbye!"
    STOP RUN.
```

#### Node.js Example

```javascript
class MainProgram {
    constructor() {
        this.dataProgram = new DataProgram();
        this.operations = new Operations(this.dataProgram);
        this.continueFlag = true; // Equivalent to CONTINUE-FLAG VALUE 'YES'
    }

    run() {
        console.log('Starting Account Management System...\n');
        
        // Equivalent to: PERFORM UNTIL CONTINUE-FLAG = 'NO'
        while (this.continueFlag) {
            this.displayMenu();
            const userChoice = this.getUserChoice();
            this.processUserChoice(userChoice);
        }
        
        // Equivalent to: DISPLAY "Exiting the program. Goodbye!"
        console.log('Exiting the program. Goodbye!');
        
        // Equivalent to: STOP RUN
        process.exit(0);
    }
    
    displayMenu() {
        console.log('--------------------------------');
        console.log('Account Management System');
        console.log('1. View Balance');
        console.log('2. Credit Account');
        console.log('3. Debit Account');
        console.log('4. Exit');
        console.log('--------------------------------');
    }
    
    processUserChoice(userChoice) {
        switch (userChoice) {
            case 1:
                // Equivalent to: CALL 'Operations' USING 'TOTAL '
                this.operations.call('TOTAL ');
                break;
            case 2:
                // Equivalent to: CALL 'Operations' USING 'CREDIT'
                this.operations.call('CREDIT');
                break;
            case 3:
                // Equivalent to: CALL 'Operations' USING 'DEBIT '
                this.operations.call('DEBIT ');
                break;
            case 4:
                // Equivalent to: MOVE 'NO' TO CONTINUE-FLAG
                this.continueFlag = false;
                break;
            default:
                // Equivalent to: DISPLAY "Invalid choice, please select 1-4."
                console.log('Invalid choice, please select 1-4.');
                break;
        }
    }
}
```

### 2. Operations Module

#### COBOL (operations.cob)

```cobol
IF OPERATION-TYPE = 'CREDIT'
    DISPLAY "Enter credit amount: "
    ACCEPT AMOUNT
    CALL 'DataProgram' USING 'READ', FINAL-BALANCE
    ADD AMOUNT TO FINAL-BALANCE
    CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
    DISPLAY "Amount credited. New balance: " FINAL-BALANCE

ELSE IF OPERATION-TYPE = 'DEBIT '
    DISPLAY "Enter debit amount: "
    ACCEPT AMOUNT
    CALL 'DataProgram' USING 'read', FINAL-BALANCE
    IF FINAL-BALANCE >= AMOUNT
        SUBTRACT AMOUNT FROM FINAL-BALANCE
        CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
        DISPLAY "Amount debited. New balance: " FINAL-BALANCE
    ELSE
        DISPLAY "Insufficient funds for this debit."
    END-IF
```

#### Node.js Operations Example

```javascript
handleCreditAccount() {
    // Equivalent to: DISPLAY "Enter credit amount: "
    console.log('Enter credit amount: ');
    
    // Equivalent to: ACCEPT AMOUNT
    const amountInput = readlineSync.question('');
    const amount = parseFloat(amountInput);
    
    // Equivalent to: CALL 'DataProgram' USING 'read', FINAL-BALANCE
    let currentBalance = this.dataProgram.call('read');
    
    // Equivalent to: ADD AMOUNT TO FINAL-BALANCE
    const newBalance = currentBalance + amount;
    
    // Equivalent to: CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
    this.dataProgram.call('write', newBalance);
    
    // Equivalent to: DISPLAY "Amount credited. New balance: " FINAL-BALANCE
    console.log(`Amount credited. New balance: ${this.dataProgram.formatBalance(newBalance)}`);
}

handleDebitAccount() {
    // Equivalent to: DISPLAY "Enter debit amount: "
    console.log('Enter debit amount: ');
    
    // Equivalent to: ACCEPT AMOUNT
    const amountInput = readlineSync.question('');
    const amount = parseFloat(amountInput);
    
    // Equivalent to: CALL 'DataProgram' USING 'read', FINAL-BALANCE
    const currentBalance = this.dataProgram.call('read');
    
    // Equivalent to: IF FINAL-BALANCE >= AMOUNT
    if (currentBalance >= amount) {
        // Equivalent to: SUBTRACT AMOUNT FROM FINAL-BALANCE
        const newBalance = currentBalance - amount;
        
        // Equivalent to: CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
        this.dataProgram.call('write', newBalance);
        
        // Equivalent to: DISPLAY "Amount debited. New balance: " FINAL-BALANCE
        console.log(`Amount debited. New balance: ${this.dataProgram.formatBalance(newBalance)}`);
    } else {
        // Equivalent to: DISPLAY "Insufficient funds for this debit."
        console.log('Insufficient funds for this debit.');
    }
}
```

### 3. Data Management

#### COBOL (data.cob)

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. DataProgram.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  STORAGE-BALANCE    PIC 9(6)V99 VALUE 1000.00.
01  OPERATION-TYPE     PIC X(6).

PROCEDURE DIVISION USING PASSED-OPERATION BALANCE.
    MOVE PASSED-OPERATION TO OPERATION-TYPE

    IF OPERATION-TYPE = 'read'
        MOVE STORAGE-BALANCE TO BALANCE
    ELSE IF OPERATION-TYPE = 'WRITE'
        MOVE BALANCE TO STORAGE-BALANCE
    END-IF
    GOBACK.
```

#### Node.js Equivalent

```javascript
class DataProgram {
    constructor() {
        // Initialize with default balance of $1,000.00 
        // (equivalent to COBOL PIC 9(6)V99 VALUE 1000.00)
        this.storageBalance = 1000.00;
    }

    call(operationType, balance = null) {
        if (operationType === 'read') {
            // Equivalent to: MOVE STORAGE-BALANCE TO BALANCE
            return this.storageBalance;
        } else if (operationType === 'write') {
            // Equivalent to: MOVE BALANCE TO STORAGE-BALANCE
            this.storageBalance = balance;
            return balance;
        }
        return this.storageBalance;
    }

    // Formats balance to match COBOL display format (6 digits + 2 decimals)
    formatBalance(amount) {
        return amount.toFixed(2).padStart(9, '0');
    }
}
```

## Data Type Mappings

| COBOL Data Type | Node.js Equivalent | Purpose |
|-----------------|-------------------|---------|
| `PIC 9(6)V99` | `number` with `.toFixed(2)` | Currency amounts with 2 decimal places |
| `PIC X(6)` | `string` | Operation type codes |
| `PIC 9` | `number` | Menu choice selections |
| `PIC X(3)` | `boolean` | Continue flag (YES/NO → true/false) |

## Statement Mappings

| COBOL Statement | Node.js Equivalent | Description |
|-----------------|-------------------|-------------|
| `ACCEPT variable` | `readlineSync.question('')` | Get user input |
| `DISPLAY message` | `console.log(message)` | Output to screen |
| `CALL 'Program' USING params` | `object.method(params)` | Call other modules |
| `MOVE value TO variable` | `variable = value` | Assign values |
| `PERFORM UNTIL condition` | `while (condition)` | Loop control |
| `EVALUATE expression` | `switch (expression)` | Multi-way branching |
| `IF condition` | `if (condition)` | Conditional execution |
| `ADD amount TO balance` | `balance += amount` | Arithmetic addition |
| `SUBTRACT amount FROM balance` | `balance -= amount` | Arithmetic subtraction |
| `GOBACK` | `return` | Return from procedure |
| `STOP RUN` | `process.exit(0)` | Terminate program |

## Business Logic Preservation

### 1. Menu Flow

- **COBOL**: `PERFORM UNTIL CONTINUE-FLAG = 'NO'` loop
- **Node.js**: `while (this.continueFlag)` loop
- **Result**: Identical behavior - menu displays until user selects exit

### 2. Balance Display Format

- **COBOL**: `PIC 9(6)V99` displays as "001000.00"
- **Node.js**: `.toFixed(2).padStart(9, '0')` produces "001000.00"
- **Result**: Exact same formatting

### 3. Insufficient Funds Check

- **COBOL**: `IF FINAL-BALANCE >= AMOUNT`
- **Node.js**: `if (currentBalance >= amount)`
- **Result**: Same overdraft protection logic

### 4. Transaction Processing

- **COBOL**: Read → Modify → Write sequence
- **Node.js**: `call('read')` → calculate → `call('write')` sequence
- **Result**: Identical transaction integrity

### 5. Error Handling

- **COBOL**: `WHEN OTHER` for invalid menu choices
- **Node.js**: `default:` case in switch statement
- **Result**: Same error message and flow

## Testing Verification

The Node.js application passes all validation tests that verify:

1. ✅ Initial balance is $1,000.00 (matches COBOL default)
2. ✅ Balance formatting matches COBOL `PIC 9(6)V99` format
3. ✅ Credit operations update balance correctly
4. ✅ Debit operations respect insufficient funds rule
5. ✅ Menu navigation works identically
6. ✅ All class structures and methods are present

## Enhanced Features in Node.js Version

While preserving exact COBOL behavior, the Node.js version adds:

1. **Enhanced Input Validation**: Checks for non-numeric inputs
2. **Better Error Messages**: More descriptive error handling
3. **Modern Structure**: Object-oriented design for maintainability
4. **Debugging Support**: VS Code launch configuration
5. **Automated Testing**: Validation script to verify conversion
6. **Documentation**: Comprehensive inline comments

## Conclusion

The Node.js conversion successfully preserves 100% of the original COBOL business logic while providing a modern, maintainable implementation that can serve as the foundation for further modernization efforts.

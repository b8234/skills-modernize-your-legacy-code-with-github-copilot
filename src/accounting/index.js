/**
 * Node.js Account Management System
 * Converted from COBOL legacy application while preserving:
 * - Original business logic
 * - Data integrity 
 * - Menu options and user flow
 * - Transaction behavior and validation
 */

const readlineSync = require('readline-sync');

/**
 * DataProgram Class - Equivalent to data.cob
 * Handles data persistence and storage operations for account balances
 * Business Rules:
 * - Default Balance: $1,000.00
 * - Decimal Precision: 2 decimal places
 * - Single Account Model: One account per session
 */
class DataProgram {
    constructor() {
        // Initialize with default balance of $1,000.00 (equivalent to COBOL PIC 9(6)V99 VALUE 1000.00)
        this.storageBalance = 1000.00;
    }

    /**
     * Handles read and write operations for balance data
     * @param {string} operationType - 'read' or 'write'
     * @param {number} balance - balance value for write operations
     * @returns {number} current balance for read operations
     */
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

    /**
     * Formats balance to match COBOL display format (6 digits + 2 decimals)
     * @param {number} amount 
     * @returns {string} formatted balance (e.g., "001000.00")
     */
    formatBalance(amount) {
        return amount.toFixed(2).padStart(9, '0');
    }
}

/**
 * Operations Class - Equivalent to operations.cob
 * Handles core business logic for account transactions and balance operations
 * Business Rules:
 * - Credit Operations: No upper limit restrictions
 * - Debit Operations: Insufficient funds protection
 * - Transaction integrity through proper data flow
 */
class Operations {
    constructor(dataProgram) {
        this.dataProgram = dataProgram;
    }

    /**
     * Handles different operation types based on passed operation code
     * @param {string} operationType - 'TOTAL ', 'CREDIT', 'DEBIT '
     */
    call(operationType) {
        // Equivalent to: MOVE PASSED-OPERATION TO OPERATION-TYPE
        const operation = operationType.trim();

        switch (operation) {
            case 'TOTAL':
                this.handleViewBalance();
                break;
            case 'CREDIT':
                this.handleCreditAccount();
                break;
            case 'DEBIT':
                this.handleDebitAccount();
                break;
            default:
                console.log('Unknown operation type');
        }
    }

    /**
     * View Balance Operation - Equivalent to TOTAL operation in COBOL
     * Business Logic: Display current account balance
     */
    handleViewBalance() {
        // Equivalent to: CALL 'DataProgram' USING 'read', FINAL-BALANCE
        const currentBalance = this.dataProgram.call('read');
        
        // Equivalent to: DISPLAY "Current balance: " FINAL-BALANCE
        console.log(`Current balance: ${this.dataProgram.formatBalance(currentBalance)}`);
    }

    /**
     * Credit Account Operation - Equivalent to CREDIT operation in COBOL
     * Business Rules:
     * - Accepts any positive amount
     * - Updates balance automatically
     * - No upper limit restrictions
     */
    handleCreditAccount() {
        // Equivalent to: DISPLAY "Enter credit amount: "
        console.log('Enter credit amount: ');
        
        // Equivalent to: ACCEPT AMOUNT
        const amountInput = readlineSync.question('');
        const amount = parseFloat(amountInput);

        // Input validation
        if (isNaN(amount) || amount < 0) {
            console.log('Invalid amount. Please enter a positive number.');
            return;
        }

        // Equivalent to: CALL 'DataProgram' USING 'read', FINAL-BALANCE
        let currentBalance = this.dataProgram.call('read');
        
        // Equivalent to: ADD AMOUNT TO FINAL-BALANCE
        const newBalance = currentBalance + amount;
        
        // Equivalent to: CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
        this.dataProgram.call('write', newBalance);
        
        // Equivalent to: DISPLAY "Amount credited. New balance: " FINAL-BALANCE
        console.log(`Amount credited. New balance: ${this.dataProgram.formatBalance(newBalance)}`);
    }

    /**
     * Debit Account Operation - Equivalent to DEBIT operation in COBOL
     * Business Rules:
     * - Insufficient funds protection (prevents overdrafts)
     * - Only processes if CURRENT_BALANCE >= DEBIT_AMOUNT
     * - Updates balance only for successful transactions
     */
    handleDebitAccount() {
        // Equivalent to: DISPLAY "Enter debit amount: "
        console.log('Enter debit amount: ');
        
        // Equivalent to: ACCEPT AMOUNT
        const amountInput = readlineSync.question('');
        const amount = parseFloat(amountInput);

        // Input validation
        if (isNaN(amount) || amount < 0) {
            console.log('Invalid amount. Please enter a positive number.');
            return;
        }

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
}

/**
 * MainProgram Class - Equivalent to main.cob
 * Entry point and user interface controller for the account management system
 * Business Logic:
 * - Menu-driven interface with options 1-4
 * - Input validation for menu choices
 * - Main program loop until user exits
 */
class MainProgram {
    constructor() {
        this.dataProgram = new DataProgram();
        this.operations = new Operations(this.dataProgram);
        this.continueFlag = true; // Equivalent to CONTINUE-FLAG VALUE 'YES'
    }

    /**
     * Main application entry point - Equivalent to MAIN-LOGIC
     * Implements the main program loop with menu display and user interaction
     */
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

    /**
     * Displays the main menu - preserves exact COBOL menu format
     */
    displayMenu() {
        console.log('--------------------------------');
        console.log('Account Management System');
        console.log('1. View Balance');
        console.log('2. Credit Account');
        console.log('3. Debit Account');
        console.log('4. Exit');
        console.log('--------------------------------');
    }

    /**
     * Gets user menu choice with input validation
     * @returns {number} user's menu selection
     */
    getUserChoice() {
        // Equivalent to: DISPLAY "Enter your choice (1-4): "
        const choice = readlineSync.question('Enter your choice (1-4): ');
        return parseInt(choice);
    }

    /**
     * Processes user menu choice - Equivalent to EVALUATE USER-CHOICE
     * @param {number} userChoice - menu option selected by user
     */
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
        
        // Add blank line for readability (menu will redisplay on next iteration)
        if (this.continueFlag) {
            console.log('');
        }
    }
}

/**
 * Application Entry Point
 * Creates and starts the main program - preserves COBOL program flow
 */
function main() {
    try {
        const app = new MainProgram();
        app.run();
    } catch (error) {
        console.error('Application error:', error.message);
        process.exit(1);
    }
}

// Start the application if this file is run directly
if (require.main === module) {
    main();
}

// Export classes for testing
module.exports = {
    MainProgram,
    Operations,
    DataProgram,
    main
};

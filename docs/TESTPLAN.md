# COBOL Account Management System - Test Plan

## Overview

This test plan covers all business logic and functionality of the COBOL account management system. It is designed to validate the current system behavior with business stakeholders and serve as a foundation for creating automated unit and integration tests in the Node.js transformation.

## Test Environment

- **System Under Test**: COBOL Account Management System
- **Initial Account Balance**: $1,000.00
- **Currency Format**: Two decimal places (e.g., 001000.00)
- **Session Type**: Single session, in-memory persistence

## Test Cases

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC001 | Application Startup and Menu Display | System is compiled and ready to run | 1. Execute ./accountsystem 2. Observe initial display | Menu displays with options 1-4: 1. View Balance 2. Credit Account 3. Debit Account 4. Exit | | | |
| TC002 | View Initial Balance | Application is running and menu is displayed | 1. Select option 1 (View Balance) 2. Observe displayed balance | Current balance displays as "001000.00" | | | Initial account balance validation |
| TC003 | Valid Credit Transaction | Application running, current balance $1,000.00 | 1. Select option 2 (Credit Account) 2. Enter amount: 250.50 3. Observe result | 1. Prompt for credit amount appears 2. New balance displays as "001250.50" 3. Confirmation message shown 4. Menu redisplays | | | Standard credit operation |
| TC004 | Credit with Zero Amount | Application running, known balance | 1. Select option 2 (Credit Account) 2. Enter amount: 0 3. Observe result | System should handle zero credit (behavior to be validated) | | | Edge case testing |
| TC005 | Credit with Large Amount | Application running, current balance $1,000.00 | 1. Select option 2 (Credit Account) 2. Enter amount: 500000.00 3. Observe result | 1. New balance displays as "501000.00" 2. No upper limit restriction 3. Transaction processes successfully | | | Maximum amount testing |
| TC006 | Credit with Decimal Places | Application running, current balance $1,000.00 | 1. Select option 2 (Credit Account) 2. Enter amount: 123.45 3. Observe result | New balance displays as "001123.45" with proper decimal handling | | | Decimal precision validation |
| TC007 | Valid Debit Transaction - Sufficient Funds | Application running, current balance $1,000.00 | 1. Select option 3 (Debit Account) 2. Enter amount: 300.00 3. Observe result | 1. Prompt for debit amount appears 2. New balance displays as "000700.00" 3. Confirmation message shown 4. Menu redisplays | | | Standard debit operation |
| TC008 | Debit Transaction - Insufficient Funds | Application running, current balance $1,000.00 | 1. Select option 3 (Debit Account) 2. Enter amount: 1500.00 3. Observe result | 1. Error message: "Insufficient funds for this debit." 2. Balance remains unchanged at "001000.00" 3. Menu redisplays | | | Overdraft protection validation |
| TC009 | Debit Transaction - Exact Balance | Application running, current balance $1,000.00 | 1. Select option 3 (Debit Account) 2. Enter amount: 1000.00 3. Observe result | 1. Transaction processes successfully 2. New balance displays as "000000.00" 3. Confirmation message shown | | | Boundary condition testing |
| TC010 | Debit with Zero Amount | Application running, known balance | 1. Select option 3 (Debit Account) 2. Enter amount: 0 3. Observe result | System should handle zero debit (behavior to be validated) | | | Edge case testing |
| TC011 | Multiple Transaction Sequence | Application running, initial balance $1,000.00 | 1. Credit $200.00 2. View balance 3. Debit $150.00 4. View balance | 1. After credit: balance = "001200.00" 2. After debit: balance = "001050.00" 3. Each operation updates balance correctly | | | Transaction persistence testing |
| TC012 | Invalid Menu Choice - Alphabetic | Application running, menu displayed | 1. Enter 'A' as menu choice 2. Observe system response | Error message: "Invalid choice, please select 1-4." Menu redisplays | | | Input validation testing |
| TC013 | Invalid Menu Choice - Out of Range | Application running, menu displayed | 1. Enter '5' as menu choice 2. Observe system response | Error message: "Invalid choice, please select 1-4." Menu redisplays | | | Input validation testing |
| TC014 | Invalid Menu Choice - Negative Number | Application running, menu displayed | 1. Enter '-1' as menu choice 2. Observe system response | Error message: "Invalid choice, please select 1-4." Menu redisplays | | | Input validation testing |
| TC015 | Normal Exit Sequence | Application running, menu displayed | 1. Select option 4 (Exit) 2. Observe system response | 1. Message: "Exiting the program. Goodbye!" 2. Program terminates cleanly 3. Control returns to command prompt | | | Normal termination testing |
| TC016 | Session Persistence - Balance Retention | Application running, performed transactions | 1. Perform credit transaction 2. Perform debit transaction 3. View balance multiple times | Balance remains consistent throughout session until program exit | | | Session state management |
| TC017 | Credit with Negative Amount | Application running, current balance known | 1. Select option 2 (Credit Account) 2. Enter amount: -100.00 3. Observe result | System behavior to be validated (may treat as positive or reject) | | | Negative input validation |
| TC018 | Debit with Negative Amount | Application running, current balance known | 1. Select option 3 (Debit Account) 2. Enter amount: -100.00 3. Observe result | System behavior to be validated (may treat as positive or reject) | | | Negative input validation |
| TC019 | Credit with Non-Numeric Input | Application running, menu displayed | 1. Select option 2 (Credit Account) 2. Enter non-numeric value (e.g., "abc") 3. Observe result | System behavior to be validated (error handling or input rejection) | | | Data type validation |
| TC020 | Debit with Non-Numeric Input | Application running, menu displayed | 1. Select option 3 (Debit Account) 2. Enter non-numeric value (e.g., "xyz") 3. Observe result | System behavior to be validated (error handling or input rejection) | | | Data type validation |
| TC021 | Maximum Balance Limit Testing | Application running, current balance $999,000.00 | 1. Select option 2 (Credit Account) 2. Enter amount: 999.99 3. Observe result | Verify system handles maximum balance ($999,999.99) correctly | | | Boundary condition testing |
| TC022 | Balance Display Format Consistency | Application running, various balance amounts | 1. Perform transactions resulting in different balance amounts 2. View balance after each transaction | All balance displays use consistent format: 6 digits + 2 decimal places (e.g., 001234.56) | | | Display format validation |
| TC023 | Menu Redisplay After Each Operation | Application running | 1. Perform any valid operation 2. Observe post-operation behavior | Menu consistently redisplays after every operation (except exit) | | | User interface flow |
| TC024 | Concurrent Operation Simulation | Application running | 1. Rapidly perform multiple operations 2. View balance between operations | System maintains data integrity and correct balance calculations | | | Data integrity testing |
| TC025 | Long Session Stability | Application running | 1. Perform 50+ mixed operations 2. Monitor system behavior | System remains stable and responsive throughout extended use | | | Stability testing |

## Test Categories

### Functional Testing

- **Menu Navigation**: TC001, TC012, TC013, TC014, TC015, TC023
- **Balance Inquiry**: TC002, TC016, TC022
- **Credit Operations**: TC003, TC004, TC005, TC006, TC017, TC019
- **Debit Operations**: TC007, TC008, TC009, TC010, TC018, TC020
- **Transaction Sequencing**: TC011, TC016, TC024

### Boundary Testing

- **Maximum Values**: TC005, TC021
- **Minimum Values**: TC004, TC009, TC010
- **Exact Boundary**: TC008, TC009

### Error Handling

- **Invalid Input**: TC012, TC013, TC014, TC017, TC018, TC019, TC020
- **Business Rule Violations**: TC008 (insufficient funds)

### Data Integrity

- **Session Persistence**: TC016, TC024
- **Format Consistency**: TC022, TC006
- **Calculation Accuracy**: TC011, TC024

### User Experience

- **Interface Flow**: TC023, TC015
- **Feedback Messages**: TC003, TC007, TC008, TC012
- **System Stability**: TC025

## Testing Notes

### For Business Stakeholder Review

1. **Critical Business Rules**: Pay special attention to TC008 (insufficient funds protection) and TC011 (transaction sequencing)
2. **User Experience**: Validate menu flow and error messaging in TC012-TC014
3. **Data Accuracy**: Confirm decimal handling and balance calculations in TC006, TC011
4. **Edge Cases**: Review system behavior for boundary conditions in TC009, TC021

### For Node.js Implementation

1. **API Endpoints**: Each menu option will likely become a REST endpoint
2. **Validation**: Implement robust input validation beyond COBOL's basic handling
3. **Error Responses**: Design consistent error response formats
4. **State Management**: Replace in-memory session with persistent storage
5. **Concurrency**: Add proper handling for concurrent transactions

### Known Limitations to Address in Node.js

- Single session limitation (TC016)
- Basic input validation (TC017-TC020)
- No transaction history
- No user authentication
- Limited error messaging

## Test Execution Instructions

1. **Setup**: Compile COBOL application using provided build instructions
2. **Execution**: Run each test case in isolation for accurate results
3. **Documentation**: Record actual results and any deviations from expected behavior
4. **Validation**: Review results with business stakeholders before Node.js implementation
5. **Traceability**: Map test cases to Node.js unit/integration tests during transformation

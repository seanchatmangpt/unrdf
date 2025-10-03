#!/bin/bash

# JIRA Feature Parity Status Script for knowd
# Usage: ./jira-status.sh [validate|report|update TICKET_ID:STATUS:NOTES]

set -e

echo "🔍 knowd JIRA Feature Parity Status Tool"
echo "========================================"

case "${1:-report}" in
    "validate")
        echo "📋 Validating feature parity..."
        ./knowd -jira-validate
        ;;
    "report")
        echo "📊 Generating feature parity report..."
        ./knowd -jira-report
        ;;
    "update")
        if [ -z "$2" ]; then
            echo "❌ Usage: $0 update TICKET_ID:STATUS:NOTES"
            echo "   Example: $0 update KNOWD-116:implemented:Added migration docs"
            exit 1
        fi
        echo "🔄 Updating ticket: $2"
        ./knowd -jira-update "$2"
        ;;
    *)
        echo "❌ Unknown command: $1"
        echo ""
        echo "Usage: $0 [validate|report|update TICKET_ID:STATUS:NOTES]"
        echo ""
        echo "Commands:"
        echo "  validate  - Validate current feature parity status"
        echo "  report    - Generate detailed feature parity report"
        echo "  update    - Update a specific ticket status"
        echo ""
        echo "Examples:"
        echo "  $0 validate"
        echo "  $0 report"
        echo "  $0 update KNOWD-116:implemented:Added migration documentation"
        exit 1
        ;;
esac

echo ""
echo "✅ Operation completed successfully!"

/**
 * Tax Processing Web Service
 * Legacy Java EJB for Taxpayer Portal Integration
 * Handles tax calculation requests from web portal
 */

package gov.irs.services.tax;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import java.math.BigDecimal;
import java.util.logging.Logger;

@Stateless
@TransactionAttribute(TransactionAttributeType.REQUIRED)
public class TaxCalculationService implements TaxCalculationRemote {
    
    private static final Logger logger = Logger.getLogger(TaxCalculationService.class.getName());
    
    /**
     * Calculates federal income tax for a taxpayer
     * Integrates with mainframe COBOL tax calculation engine
     * 
     * @param taxRequest Contains taxpayer information and income details
     * @return TaxCalculationResult with calculated taxes and credits
     */
    public TaxCalculationResult calculateFederalTax(TaxCalculationRequest taxRequest) {
        
        logger.info("Processing tax calculation for taxpayer: " + taxRequest.getTaxpayerId());
        
        TaxCalculationResult result = new TaxCalculationResult();
        result.setTaxpayerId(taxRequest.getTaxpayerId());
        result.setTaxYear(taxRequest.getTaxYear());
        
        try {
            // Validate input data
            validateTaxRequest(taxRequest);
            
            // Calculate standard deduction based on filing status
            BigDecimal standardDeduction = calculateStandardDeduction(
                taxRequest.getFilingStatus(), 
                taxRequest.isAge65OrOlder()
            );
            result.setStandardDeduction(standardDeduction);
            
            // Calculate taxable income
            BigDecimal taxableIncome = calculateTaxableIncome(
                taxRequest.getGrossIncome(), 
                standardDeduction
            );
            result.setTaxableIncome(taxableIncome);
            
            // Calculate federal tax using bracket system
            BigDecimal federalTax = calculateFederalTaxAmount(taxableIncome);
            result.setFederalTax(federalTax);
            
            // Calculate child tax credit
            BigDecimal childCredit = calculateChildTaxCredit(
                taxRequest.getNumberOfChildren(),
                taxRequest.getGrossIncome()
            );
            result.setChildTaxCredit(childCredit);
            
            // Calculate earned income credit
            BigDecimal earnedIncomeCredit = calculateEarnedIncomeCredit(
                taxRequest.getGrossIncome(),
                taxRequest.getNumberOfChildren(),
                taxRequest.getFilingStatus()
            );
            result.setEarnedIncomeCredit(earnedIncomeCredit);
            
            // Calculate total credits
            BigDecimal totalCredits = childCredit.add(earnedIncomeCredit);
            result.setTotalCredits(totalCredits);
            
            // Calculate net tax due
            BigDecimal netTaxDue = federalTax.subtract(totalCredits);
            if (netTaxDue.compareTo(BigDecimal.ZERO) < 0) {
                netTaxDue = BigDecimal.ZERO;
            }
            result.setNetTaxDue(netTaxDue);
            
            // Set calculation status
            result.setCalculationStatus("SUCCESS");
            result.setStatusMessage("Tax calculation completed successfully");
            
            logger.info("Tax calculation completed for taxpayer: " + taxRequest.getTaxpayerId() + 
                       ", Net tax due: " + netTaxDue);
            
        } catch (Exception e) {
            logger.severe("Error calculating tax for taxpayer: " + taxRequest.getTaxpayerId() + 
                         ", Error: " + e.getMessage());
            
            result.setCalculationStatus("ERROR");
            result.setStatusMessage("Error processing tax calculation: " + e.getMessage());
        }
        
        return result;
    }
    
    private void validateTaxRequest(TaxCalculationRequest request) throws TaxCalculationException {
        if (request.getTaxpayerId() == null || request.getTaxpayerId().trim().length() == 0) {
            throw new TaxCalculationException("Taxpayer ID is required");
        }
        
        if (request.getGrossIncome() == null || request.getGrossIncome().compareTo(BigDecimal.ZERO) < 0) {
            throw new TaxCalculationException("Valid gross income is required");
        }
        
        if (request.getFilingStatus() == null) {
            throw new TaxCalculationException("Filing status is required");
        }
        
        if (request.getNumberOfChildren() < 0) {
            throw new TaxCalculationException("Number of children cannot be negative");
        }
    }
    
    private BigDecimal calculateStandardDeduction(String filingStatus, boolean isAge65OrOlder) {
        BigDecimal standardDeduction;
        
        if ("SINGLE".equals(filingStatus)) {
            standardDeduction = new BigDecimal("12950.00");
        } else if ("MARRIED_FILING_JOINT".equals(filingStatus)) {
            standardDeduction = new BigDecimal("25900.00");
        } else if ("MARRIED_FILING_SEPARATE".equals(filingStatus)) {
            standardDeduction = new BigDecimal("12950.00");
        } else if ("HEAD_OF_HOUSEHOLD".equals(filingStatus)) {
            standardDeduction = new BigDecimal("19400.00");
        } else {
            standardDeduction = new BigDecimal("12950.00"); // Default to single
        }
        
        // Additional standard deduction for age 65 or older
        if (isAge65OrOlder) {
            standardDeduction = standardDeduction.add(new BigDecimal("1400.00"));
        }
        
        return standardDeduction;
    }
    
    private BigDecimal calculateTaxableIncome(BigDecimal grossIncome, BigDecimal standardDeduction) {
        BigDecimal taxableIncome = grossIncome.subtract(standardDeduction);
        return taxableIncome.compareTo(BigDecimal.ZERO) > 0 ? taxableIncome : BigDecimal.ZERO;
    }
    
    private BigDecimal calculateFederalTaxAmount(BigDecimal taxableIncome) {
        // Simplified tax bracket calculation
        // 10% on first $10,275
        // 12% on next $31,500 ($10,276 to $41,775)
        // 22% on remainder (for this example)
        
        BigDecimal tax = BigDecimal.ZERO;
        BigDecimal bracket1Limit = new BigDecimal("10275");
        BigDecimal bracket2Limit = new BigDecimal("41775");
        
        if (taxableIncome.compareTo(bracket1Limit) <= 0) {
            tax = taxableIncome.multiply(new BigDecimal("0.10"));
        } else if (taxableIncome.compareTo(bracket2Limit) <= 0) {
            tax = bracket1Limit.multiply(new BigDecimal("0.10"))
                .add(taxableIncome.subtract(bracket1Limit).multiply(new BigDecimal("0.12")));
        } else {
            tax = bracket1Limit.multiply(new BigDecimal("0.10"))
                .add(new BigDecimal("31500").multiply(new BigDecimal("0.12")))
                .add(taxableIncome.subtract(bracket2Limit).multiply(new BigDecimal("0.22")));
        }
        
        return tax.setScale(2, BigDecimal.ROUND_HALF_UP);
    }
    
    private BigDecimal calculateChildTaxCredit(int numberOfChildren, BigDecimal grossIncome) {
        if (numberOfChildren <= 0) {
            return BigDecimal.ZERO;
        }
        
        BigDecimal creditPerChild = new BigDecimal("2000.00");
        BigDecimal totalCredit = creditPerChild.multiply(new BigDecimal(numberOfChildren));
        
        // Phase out credit for high-income taxpayers (simplified)
        BigDecimal phaseOutThreshold = new BigDecimal("200000");
        if (grossIncome.compareTo(phaseOutThreshold) > 0) {
            // Reduce credit for high income (simplified calculation)
            BigDecimal reduction = grossIncome.subtract(phaseOutThreshold)
                .divide(new BigDecimal("1000"), 0, BigDecimal.ROUND_UP)
                .multiply(new BigDecimal("50"));
            totalCredit = totalCredit.subtract(reduction);
            
            if (totalCredit.compareTo(BigDecimal.ZERO) < 0) {
                totalCredit = BigDecimal.ZERO;
            }
        }
        
        return totalCredit;
    }
    
    private BigDecimal calculateEarnedIncomeCredit(BigDecimal grossIncome, int numberOfChildren, String filingStatus) {
        // Simplified EIC calculation
        if (numberOfChildren <= 0 || grossIncome.compareTo(new BigDecimal("50000")) >= 0) {
            return BigDecimal.ZERO;
        }
        
        BigDecimal eicRate;
        BigDecimal maxCredit;
        
        if (numberOfChildren == 1) {
            eicRate = new BigDecimal("0.34");
            maxCredit = new BigDecimal("3618");
        } else if (numberOfChildren == 2) {
            eicRate = new BigDecimal("0.40");
            maxCredit = new BigDecimal("5980");
        } else {
            eicRate = new BigDecimal("0.45");
            maxCredit = new BigDecimal("6728");
        }
        
        BigDecimal calculatedCredit = grossIncome.multiply(eicRate);
        
        return calculatedCredit.compareTo(maxCredit) > 0 ? maxCredit : calculatedCredit;
    }
}

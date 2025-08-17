/**
 * Government Citizen Services Portal
 * Legacy Java Enterprise Application (circa 2005)
 * 
 * CitizenServiceBean.java
 * Handles citizen benefit applications and status inquiries
 */

package gov.agency.services.citizen;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

@Stateless
@TransactionAttribute(TransactionAttributeType.REQUIRED)
public class CitizenServiceBean implements CitizenServiceRemote {
    
    private static final Logger logger = Logger.getLogger(CitizenServiceBean.class.getName());
    
    // Legacy database connection - managed by application server
    private DatabaseConnectionManager dbManager;
    
    /**
     * Processes benefit application for a citizen
     * 
     * @param citizenId Government-issued citizen identifier
     * @param benefitType Type of benefit being requested
     * @param applicationData XML string containing application details
     * @return Application reference number
     */
    public String processBenefitApplication(String citizenId, String benefitType, String applicationData) {
        
        Connection conn = null;
        PreparedStatement pstmt = null;
        String applicationNumber = null;
        
        try {
            // Validate citizen exists in system
            if (!validateCitizenExists(citizenId)) {
                throw new CitizenServiceException("Citizen ID not found: " + citizenId);
            }
            
            // Check for duplicate applications
            if (hasPendingApplication(citizenId, benefitType)) {
                throw new CitizenServiceException("Pending application already exists for citizen: " + citizenId);
            }
            
            // Generate application number using legacy format
            applicationNumber = generateApplicationNumber(benefitType);
            
            // Insert application into database
            conn = dbManager.getConnection();
            String insertSQL = "INSERT INTO BENEFIT_APPLICATIONS " +
                             "(APPLICATION_NUMBER, CITIZEN_ID, BENEFIT_TYPE, " +
                             "APPLICATION_DATA, STATUS, CREATED_DATE) " +
                             "VALUES (?, ?, ?, ?, 'PENDING', CURRENT_TIMESTAMP)";
            
            pstmt = conn.prepareStatement(insertSQL);
            pstmt.setString(1, applicationNumber);
            pstmt.setString(2, citizenId);
            pstmt.setString(3, benefitType);
            pstmt.setString(4, applicationData);
            
            int rowsInserted = pstmt.executeUpdate();
            
            if (rowsInserted != 1) {
                throw new CitizenServiceException("Failed to insert application record");
            }
            
            // Log successful application submission
            logger.info("Benefit application submitted - Citizen: " + citizenId + 
                       ", Type: " + benefitType + ", App#: " + applicationNumber);
            
            // Trigger batch processing workflow (legacy mainframe integration)
            triggerBatchProcessing(applicationNumber);
            
        } catch (SQLException e) {
            logger.log(Level.SEVERE, "Database error processing application", e);
            throw new CitizenServiceException("System error processing application", e);
        } catch (Exception e) {
            logger.log(Level.SEVERE, "Unexpected error processing application", e);
            throw new CitizenServiceException("System error processing application", e);
        } finally {
            // Clean up database resources
            if (pstmt != null) {
                try { pstmt.close(); } catch (SQLException e) { /* ignore */ }
            }
            if (conn != null) {
                try { conn.close(); } catch (SQLException e) { /* ignore */ }
            }
        }
        
        return applicationNumber;
    }
    
    /**
     * Retrieves application status for a citizen
     */
    public List<ApplicationStatus> getApplicationStatus(String citizenId) {
        
        Connection conn = null;
        PreparedStatement pstmt = null;
        ResultSet rs = null;
        List<ApplicationStatus> applications = new ArrayList<ApplicationStatus>();
        
        try {
            conn = dbManager.getConnection();
            String selectSQL = "SELECT APPLICATION_NUMBER, BENEFIT_TYPE, STATUS, " +
                             "CREATED_DATE, LAST_UPDATED " +
                             "FROM BENEFIT_APPLICATIONS " +
                             "WHERE CITIZEN_ID = ? " +
                             "ORDER BY CREATED_DATE DESC";
            
            pstmt = conn.prepareStatement(selectSQL);
            pstmt.setString(1, citizenId);
            rs = pstmt.executeQuery();
            
            while (rs.next()) {
                ApplicationStatus status = new ApplicationStatus();
                status.setApplicationNumber(rs.getString("APPLICATION_NUMBER"));
                status.setBenefitType(rs.getString("BENEFIT_TYPE"));
                status.setStatus(rs.getString("STATUS"));
                status.setCreatedDate(rs.getTimestamp("CREATED_DATE"));
                status.setLastUpdated(rs.getTimestamp("LAST_UPDATED"));
                
                applications.add(status);
            }
            
        } catch (SQLException e) {
            logger.log(Level.SEVERE, "Database error retrieving application status", e);
            throw new CitizenServiceException("System error retrieving application status", e);
        } finally {
            // Clean up database resources
            if (rs != null) {
                try { rs.close(); } catch (SQLException e) { /* ignore */ }
            }
            if (pstmt != null) {
                try { pstmt.close(); } catch (SQLException e) { /* ignore */ }
            }
            if (conn != null) {
                try { conn.close(); } catch (SQLException e) { /* ignore */ }
            }
        }
        
        return applications;
    }
    
    private boolean validateCitizenExists(String citizenId) {
        // Implementation would check against citizen registry
        // Simplified for example purposes
        return citizenId != null && citizenId.length() == 9;
    }
    
    private boolean hasPendingApplication(String citizenId, String benefitType) {
        // Implementation would check for existing pending applications
        // Simplified for example purposes
        return false;
    }
    
    private String generateApplicationNumber(String benefitType) {
        // Legacy application number format: YYYYMMDD-XXXX-TYPE
        // Implementation would generate unique sequential number
        return "20240817-0001-" + benefitType.substring(0, 3).toUpperCase();
    }
    
    private void triggerBatchProcessing(String applicationNumber) {
        // Legacy integration - writes to mainframe processing queue
        // Implementation would interface with MQ Series or similar
        logger.info("Triggered batch processing for application: " + applicationNumber);
    }
}

/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*! 
 * \file input_subsidy.cpp
 * \ingroup Objects
 * \brief The InputNetSubsidy class source file.
 * \author Sonny Kim
 */

#include "util/base/include/definitions.h"
#include <cmath>

#include "functions/include/input_net_subsidy.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/tech_vector_parse_helper.h"
#include "technologies/include/icapture_component.h"
#include "functions/include/icoefficient.h"
#include "functions/include/efficiency.h"
#include "functions/include/intensity.h"
#include "containers/include/market_dependency_finder.h"
#include "containers/include/iinfo.h"
#include "functions/include/function_utils.h"
#include "util/logger/include/ilogger.h"

using namespace std;

extern Scenario* scenario;

// static initialize.
const string InputNetSubsidy::XML_REPORTING_NAME = "input-net-subsidy";

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \author Sonny Kim
* \return The constant XML_NAME as a static.
*/
const string& InputNetSubsidy::getXMLNameStatic() {
    const static string XML_NAME = "input-net-subsidy";
    return XML_NAME;
}

/*! \brief Get the XML name for reporting to XML file.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for reporting outputs and can be easily
* changed.
* \author Sonny Kim
* \return The constant XML_NAME.
*/
const string& InputNetSubsidy::getXMLReportingName() const{
    return XML_REPORTING_NAME;
}

const string& InputNetSubsidy::getXMLName() const {
    return getXMLNameStatic();
}

//! Constructor
//InputNetSubsidy::InputNetSubsidy():
//mAdjustedCoefficients( Value( 1.0 ) )
//{
//}

//! Constructor
InputNetSubsidy::InputNetSubsidy()
{
    TechVectorParseHelper<Value>::setDefaultValue(Value(1.0), mAdjustedCoefficients);
}

/*!
 * \brief Destructor.
 * \note An explicit constructor must be defined to avoid the compiler inlining
 *       it in the header file before the header file for the type contained in
 *       the auto_ptr is included.
 */
InputNetSubsidy::~InputNetSubsidy() {
}

/*!
 * \brief Copy constructor.
 * \note This class requires a copy constructor because it has dynamically
 *          allocated memory.
 * \param aOther subsidy input from which to copy.
 */
InputNetSubsidy::InputNetSubsidy( const InputNetSubsidy& aOther )
{
    MiniCAMInput::copy( aOther );
    // Do not clone the input coefficient as the calculated
    // coeffient will be filled out later.

    // Do not copy calibration values into the future
    // as they are only valid for one period.
    mName = aOther.mName;
    
    // copy keywords
    mKeywordMap = aOther.mKeywordMap;
}

InputNetSubsidy* InputNetSubsidy::clone() const {
    return new InputNetSubsidy( *this );
}

bool InputNetSubsidy::isSameType( const string& aType ) const {
    return aType == getXMLNameStatic();
}

void InputNetSubsidy::toDebugXML( const int aPeriod,
                               ostream& aOut,
                               Tabs* aTabs ) const
{
    XMLWriteOpeningTag ( getXMLNameStatic(), aOut, aTabs, mName );
    XMLWriteElement( mAdjustedCoefficients[ aPeriod ], "current-coef", aOut, aTabs );
    XMLWriteElement( mPhysicalDemand[ aPeriod ], "physical-demand", aOut, aTabs );
    XMLWriteElement(mSectorName, "sector-name", aOut, aTabs);
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void InputNetSubsidy::completeInit( const string& aRegionName,
                                 const string& aSectorName,
                                 const string& aSubsectorName,
                                 const string& aTechName,
                                 const IInfo* aTechInfo )
{
    //mSectorName = aSectorName;
}

void InputNetSubsidy::initCalc( const string& aRegionName,
                             const string& aSectorName,
                             const bool aIsNewInvestmentPeriod,
                             const bool aIsTrade,
                             const IInfo* aTechInfo,
                             const int aPeriod )
{
    // There must be a valid region name.
    assert( !aRegionName.empty() );
    mAdjustedCoefficients[ aPeriod ] = 1.0;
}

void InputNetSubsidy::copyParam( const IInput* aInput,
                             const int aPeriod )
{
    
}

void InputNetSubsidy::copyParamsInto( InputNetSubsidy& aInput,
                                  const int aPeriod ) const
{
    // do nothing 
}


double InputNetSubsidy::getCO2EmissionsCoefficient( const string& aGHGName,
                                             const int aPeriod ) const
{
    return 0;
}

double InputNetSubsidy::getPhysicalDemand( const int aPeriod ) const {
 
    return 0;
}

double InputNetSubsidy::getCarbonContent( const int aPeriod ) const {
    return 0;
}

void InputNetSubsidy::setPhysicalDemand( double aPhysicalDemand,
                                     const string& aRegionName,
                                     const int aPeriod )
{

}

double InputNetSubsidy::getCoefficient( const int aPeriod ) const {
    // Check that the coefficient has been initialized.
    assert( mAdjustedCoefficients[ aPeriod ].isInited() );

    return mAdjustedCoefficients[ aPeriod ];
}

void InputNetSubsidy::setCoefficient( const double aCoefficient,
                                  const int aPeriod )
{
    // Do nothing.
}

double InputNetSubsidy::getPrice( const string& aRegionName,
                              const int aPeriod ) const
{
    // Return negative of price to reflect subsidy for portfolio
    // standard market.
    // A high subsidy increases supply.
    //maw May 2017  here is where we use marketinfo to get the subsidy price
    // get a pointer to marketplace.
    Marketplace* marketplace = scenario->getMarketplace();

    //get a marketinfo object storing the net subsidy as a positive value

    IInfo* marketInfo = marketplace->getMarketInfo( mSectorName, aRegionName, aPeriod, true );

    double netSectorSubsidy = marketInfo->getDouble( "netSectorSubsidy", true );
    
    //cout << netSectorSubsidy << "\n";

    return - netSectorSubsidy;
}

void InputNetSubsidy::setPrice( const string& aRegionName,
                            const double aPrice,
                            const int aPeriod )
{
    // Not hooking this up yet, it could work.
}

double InputNetSubsidy::getCalibrationQuantity( const int aPeriod ) const
{
    return -1;
}

bool InputNetSubsidy::hasTypeFlag( const int aTypeFlag ) const {
    //  Intentionally leave this as SUBSIDY as it is just a special case of subisdy
    return ( ( aTypeFlag & ~IInput::SUBSIDY ) == 0 );
}

double InputNetSubsidy::getIncomeElasticity( const int aPeriod ) const {
    return 0;
}

double InputNetSubsidy::getPriceElasticity( const int aPeriod ) const {
    return 0;
}

double InputNetSubsidy::getTechChange( const int aPeriod ) const
{
    return 0;
}

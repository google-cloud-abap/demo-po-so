sap.ui.define(["sap/ui/test/opaQunit","./pages/App","./pages/PurchaseOrder"],function(e){"use strict";QUnit.module("Navigation Journey");e("Should see the initial page of the app",function(e,i,p){e.iStartMyApp();p.onTheAppPage.iShouldSeeTheApp();p.onTheV+
iewPage.iShouldSeeThePageView();p.iTeardownMyApp()})});                                                                                                                                                                                                        
//# sourceMappingURL=NavigationJourney.js.map                                                                                                                                                                                                                  
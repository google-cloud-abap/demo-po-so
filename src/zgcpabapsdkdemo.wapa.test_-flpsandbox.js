sap.ui.define(["sap/base/util/ObjectPath","sap/ushell/services/Container"],function(e){"use strict";e.set(["sap-ushell-config"],{defaultRenderer:"fiori2",bootstrapPlugins:{RuntimeAuthoringPlugin:{component:"sap.ushell.plugins.rta",config:{validateAppVers+
ion:false}},PersonalizePlugin:{component:"sap.ushell.plugins.rta-personalize",config:{validateAppVersion:false}}},renderers:{fiori2:{componentData:{config:{enableSearch:false,rootIntent:"Shell-home"}}}},services:{LaunchPage:{adapter:{config:{groups:[{til+
es:[{tileType:"sap.ushell.ui.tile.StaticTile",properties:{title:"Google Cloud ABAP SDK Demo",targetURL:"#comgooglegooglecloudabapsdkdemo-display"}}]}]}}},ClientSideTargetResolution:{adapter:{config:{inbounds:{"comgooglegooglecloudabapsdkdemo-display":{se+
manticObject:"comgooglegooglecloudabapsdkdemo",action:"display",description:"Google Cloud ABAP SDK Demo",title:"Google Cloud ABAP SDK Demo",signature:{parameters:{}},resolutionResult:{applicationType:"SAPUI5",additionalInformation:"SAPUI5.Component=com.g+
oogle.googlecloudabapsdkdemo",url:sap.ui.require.toUrl("com/google/googlecloudabapsdkdemo")}}}}}},NavTargetResolution:{config:{enableClientSideTargetResolution:true}}}});var o={init:function(){if(!this._oBootstrapFinished){this._oBootstrapFinished=sap.us+
hell.bootstrap("local");this._oBootstrapFinished.then(function(){sap.ushell.Container.createRenderer().placeAt("content")})}return this._oBootstrapFinished}};return o});                                                                                      
//# sourceMappingURL=flpSandbox.js.map                                                                                                                                                                                                                         
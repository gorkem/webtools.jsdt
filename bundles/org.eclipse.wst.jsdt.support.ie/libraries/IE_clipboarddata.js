/**  * Object clipboardData()  * @type clipboardData  * @super Object  * @class clipboardData  * @since JScript 5.6  * @link http://msdn2.microsoft.com/en-us/library/ms535220.aspx*/function clipboardData(){};clipboardData.prototype= new Object();/**  * function clearData(sDataFormat)  * @param sDataFormat String  * @class clipboardData  * @since JScript 5.6  * @link http://msdn2.microsoft.com/en-us/library/ms536352.aspx*/clipboardData.prototype.clearData=function(sDataFormat){};/**  * Property dataTransfer  * @type dataTransfer  * @return dataTransfer  * @class clipboardData  * @since JScript 5.6  * @link http://msdn2.microsoft.com/en-us/library/ms535861.aspx*/clipboardData.prototype.dataTransfer= new dataTransfer();/**  * function getData(sDataFormat)  * @param sDataFormat String  * @type String  * @return String  * @class clipboardData  * @since JScript 5.6  * @link http://msdn2.microsoft.com/en-us/library/ms536436.aspx*/clipboardData.prototype.getData=function(sDataFormat){};/**  * function setData(sDataFormat,sData)  * @param sDataFormat String  * @param sData String  * @type Boolean  * @return Boolean  * @class clipboardData  * @since JScript 5.6  * @link http://msdn2.microsoft.com/en-us/library/ms536744.aspx*/clipboardData.prototype.setData=function(sDataFormat,sData){};
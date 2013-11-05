<head>
<SCRIPT LANGUAGE="JavaScript">

var theImages = new Array() 

theImages[0] = '<img class="atvi-image-image" alt=""src="http://www.rilab.org/images/image01.jpg" title="" height="150">'
theImages[1] = '<img class="atvi-image-image" alt="" src="http://www.rilab.org/images/image02.jpg" title="" height="150">'
theImages[2] = '<img class="atvi-image-image" alt="" src="http://www.rilab.org/images/image03.jpg" title="" height="150">'
theImages[3] = '<img class="atvi-image-image" alt="" src="http://www.rilab.org/images/image04.jpg" title="" height="150">'
theImages[4] = '<img class="atvi-image-image" alt="" src="http://www.rilab.org/images/image05.jpg" title="" height="150">'
theImages[5] = '<img class="atvi-image-image" alt="" src="http://www.rilab.org/images/image06.jpg" title="" height="150">'
theImages[6] = '<img class="atvi-image-image" alt="" src="http://www.rilab.org/images/image07.jpg" title="" height="150">'
theImages[7] = '<img class="atvi-image-image" alt="" src="http://www.rilab.org/images/image08.jpg" title="" height="150">'
theImages[8] = '<img class="atvi-image-image" alt="" src="http://www.rilab.org/images/image09.jpg" title="" height="150">'
theImages[9] = '<img class="atvi-image-image" alt="" src="http://www.rilab.org/images/image011.jpg" title="" height="150">'

var j = 0
var p = theImages.length;
var preBuffer = new Array()
for (i = 0; i < p; i++){
preBuffer[i] = new Image()
preBuffer[i].src = theImages[i]
}
var whichImage = Math.round(Math.random()*(p-1));
function showImage(){
document.write(theImages[whichImage]);
}
</script>
</head>
<body>
<SCRIPT LANGUAGE="JavaScript">
showImage();
</script>
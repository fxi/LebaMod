


function onEachFeatureGetProp(feature, layer) {
            var dataList =''
            var dataAll=''
            var keysProp=Object.keys(feature.properties)
            var uls="<ul>"
            var ule="</ul>"
            var lis="<li>"
            var lie="</li>"
            for (i=0;i<keysProp.length;i++){
              var k=keysProp[i]
                var v= feature.properties[keysProp[i]]
                dataList += lis+k+"="+v+lie         
            }
            dataAll=uls+dataList+ule
            console.log(dataAll)
            
            layer.bindPopup(dataAll);
        
    }
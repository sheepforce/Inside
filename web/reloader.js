function reloadGraph() {
   var now = new Date();

   document.images['coldion'].src = 'coldion.svg?' + now.getTime();
   document.images['lakeshorea'].src = 'lakeshorea.svg?' + now.getTime();
   document.images['lakeshoreb'].src = 'lakeshoreb.svg?' + now.getTime();
   document.images['graphixthree1a'].src = 'graphixthree1a.svg?' + now.getTime();
   document.images['graphixthree1b'].src = 'graphixthree1b.svg?' + now.getTime();
   document.images['graphixthree1c'].src = 'graphixthree1c.svg?' + now.getTime();
   document.images['graphixthree2a'].src = 'graphixthree2a.svg?' + now.getTime();
   document.images['graphixthree2b'].src = 'graphixthree2b.svg?' + now.getTime();
   document.images['graphixthree2c'].src = 'graphixthree2c.svg?' + now.getTime();


   // Start new timer (1 sec)
   timeoutID = setTimeout('reloadGraph()', 1000);
}

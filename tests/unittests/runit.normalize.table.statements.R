test.normalize.table.statements = function() {
    normalize.table.statements = RNMImport:::normalize.table.statements

    tablestatement = data.frame(
        File = c('sdtab1', 'patab1'),
        Columns = c('ID,TIME,IPRED,IWRES,DV', 'ID,CL,Q,V2,V3,KA,ETA1,ETA2,ETA3,ETA4,AMT'),
        NoHeader = c(F,F),
        firstOnly = c(F,T),
        append = c(T,F), stringsAsFactors=FALSE)

    checkEquals(normalize.table.statements(tablestatement), tablestatement)
    
    table1 = data.frame(
        File = c('sdtab1', 'sdtab1'),
        Columns = c('ID,TIME,IPRED,IWRES,DV', 'ID,CL,Q,V2,V3,KA,ETA1,ETA2,ETA3,ETA4,AMT'),
        NoHeader = c(F,F),
        firstOnly = c(F,T),
        append = c(T,F), stringsAsFactors=FALSE)

    table2 = table1
    table2$File = c('sdtab1__1','sdtab1__2')
    attr(table2,'need.remove') = c(table2$File)
    checkEquals(normalize.table.statements(table1), table1)
    checkEquals(normalize.table.statements(table1,force.change=T), table2)

    table3 = rbind(tablestatement, table1)
    table4 = table3
    table4$File = c('sdtab1__1','patab1','sdtab1__2','sdtab1__3')
    attr(table4,'need.remove') = c('sdtab1__1','sdtab1__2','sdtab1__3')
    checkEquals(normalize.table.statements(table3), table3)
    checkEquals(normalize.table.statements(table3,force.change=T), table4)

    table5 = rbind(table3, tablestatement[2,])
    table6 = table5
    table6$File = c('sdtab1__1','patab1__1','sdtab1__2','sdtab1__3', 'patab1__2')
    attr(table6,'need.remove') = c('sdtab1__1','patab1__1','sdtab1__2','sdtab1__3', 'patab1__2')
    checkEquals(normalize.table.statements(table5), table5)
    checkEquals(normalize.table.statements(table5,force.change=T), table6)

}

$PBExportHeader$uo_doctosarribo.sru
forward
global type uo_doctosarribo from uo_admdoctos
end type
end forward

global type uo_doctosarribo from uo_admdoctos
end type
global uo_doctosarribo uo_doctosarribo

forward prototypes
public function boolean grabaimagen (datawindow adw, long fila, transaction at_transaccion, string as_archivo)
public function blob seleccionadocto (datawindow adw, long fila, ref string as_file_tmp, transaction at_transaccion)
end prototypes

public function boolean grabaimagen (datawindow adw, long fila, transaction at_transaccion, string as_archivo);Boolean	lb_Retorno = True
Long		ll_File, ll_numero
Blob 		lbl_data, lbl_temp
Integer	li_cliente, li_especie, li_Codigo

ll_numero		=	adw.Object.rear_numero[Fila]
li_cliente		=	adw.Object.clie_codigo[Fila]
li_especie	=	adw.Object.espe_codigo[Fila]

ll_File = FileOpen(as_archivo, StreamMode!)

Do While FileRead(ll_file, lbl_temp) > 0
	lbl_data += lbl_temp
Loop

FileClose(ll_file)

If ll_File = 1 Then
	FileRead(ll_file, lbl_data)
	FileClose(ll_file)
	at_Transaccion.AutoCommit = True
	
	UpDateBlob dba.ctlcalreportearribo
			Set rear_doctos  = :lbl_data 
			Where rear_numero  = :ll_numero
		    		And espe_codigo = :li_especie
    				And clie_codigo = :li_cliente
			Using at_Transaccion;
			
	at_Transaccion.AutoCommit = False
ELSE
	lb_Retorno = False
END IF

IF at_Transaccion.SQLNRows > 0 THEN
	Commit;
ELSE
	lb_Retorno = False
END IF

FileClose(ll_file)

Return lb_Retorno = False
end function

public function blob seleccionadocto (datawindow adw, long fila, ref string as_file_tmp, transaction at_transaccion);Blob 		lblob_file
Long		ll_Numero
Integer	li_Cliente, li_Especie, li_Codigo

SetNull(lblob_file)


as_file_tmp	=	adw.Object.rear_archiv[Fila]
ll_numero		=	adw.Object.rear_numero[Fila]
li_cliente		=	adw.Object.clie_codigo[Fila]
li_especie	=	adw.Object.espe_codigo[Fila]

SELECTBLOB  rear_doctos
INTO :lblob_file
FROM dba.ctlcalreportearribo
WHERE rear_numero  =:ll_numero
  and espe_codigo  = :li_especie
  and clie_codigo 	= :li_cliente
USING at_transaccion;

If at_transaccion.Sqlcode = -1 OR at_transaccion.Sqlcode = 100 Then
	MessageBox( "Atención", "Ha ocurrido el error: ~n~r" + at_transaccion.SqlErrText ) 
	Return lblob_file
End If

Return lblob_file
end function

on uo_doctosarribo.create
call super::create
end on

on uo_doctosarribo.destroy
call super::destroy
end on


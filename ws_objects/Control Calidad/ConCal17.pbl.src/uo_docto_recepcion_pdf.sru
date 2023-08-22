$PBExportHeader$uo_docto_recepcion_pdf.sru
forward
global type uo_docto_recepcion_pdf from uo_admdoctos
end type
end forward

global type uo_docto_recepcion_pdf from uo_admdoctos
end type
global uo_docto_recepcion_pdf uo_docto_recepcion_pdf

forward prototypes
public function boolean grabaimagen (datawindow adw, long fila, transaction at_transaccion, string as_archivo)
public function blob seleccionadocto (datawindow adw, long fila, ref string as_file_tmp, transaction at_transaccion)
end prototypes

public function boolean grabaimagen (datawindow adw, long fila, transaction at_transaccion, string as_archivo);Boolean	lb_Retorno = True
Long		ll_File, ll_productor
Date		ld_Fecha
Blob 		lbl_data, lbl_temp
Integer	li_cliente, li_especie, li_Planta

ll_Productor	=	adw.Object.prod_codigo[Fila]
li_Cliente		=	adw.Object.clie_codigo[Fila]
li_Especie	=	adw.Object.espe_codigo[Fila]
li_Planta		=	adw.Object.plde_codigo[Fila]
ld_Fecha		=	adw.Object.recp_fechap[Fila]

ll_File = FileOpen(as_archivo, StreamMode!)

Do While FileRead(ll_file, lbl_temp) > 0
	lbl_data += lbl_temp
Loop

FileClose(ll_file)

If ll_File = 1 Then
	FileRead(ll_file, lbl_data)
	FileClose(ll_file)
	at_Transaccion.AutoCommit = True
	
	UpDateBlob dba.ctlcal_recepcionpdf 
			Set recp_imagen  = :lbl_data 
			Where prod_codigo = :ll_productor
		    		And espe_codigo = :li_especie
    				And clie_codigo = :li_cliente
    				And plde_codigo = :li_Planta
				And recp_fechap = :ld_Fecha
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
Long		ll_Productor
Date		ld_Fecha
Integer	li_Cliente, li_especie, li_Planta

SetNull(lblob_file)

as_file_tmp	=	adw.Object.reid_archiv[Fila]

ll_Productor	=	adw.Object.prod_codigo[Fila]
li_Cliente		=	adw.Object.clie_codigo[Fila]
li_Especie	=	adw.Object.espe_codigo[Fila]
li_Planta		=	adw.Object.plde_codigo[Fila]
ld_Fecha		=	adw.Object.recp_fechap[Fila]

SELECTBLOB  recp_imagen
INTO :lblob_file
FROM dba.dba.ctlcal_recepcionpdf
WHERE prod_codigo  =:ll_Productor
  and espe_codigo  = :li_especie
  and clie_codigo 	= :li_cliente
  and plde_codigo  = :li_Planta
  and recp_fechap	=	:ld_Fecha
USING at_transaccion;

If at_transaccion.Sqlcode = -1 OR at_transaccion.Sqlcode = 100 Then
	MessageBox( "Atención", "Ha ocurrido el error: ~n~r" + at_transaccion.SqlErrText ) 
	Return lblob_file
End If

Return lblob_file
end function

on uo_docto_recepcion_pdf.create
call super::create
end on

on uo_docto_recepcion_pdf.destroy
call super::destroy
end on


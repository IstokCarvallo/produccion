$PBExportHeader$uo_aprobacioncompactospdf.sru
forward
global type uo_aprobacioncompactospdf from uo_admdoctos
end type
end forward

global type uo_aprobacioncompactospdf from uo_admdoctos
end type
global uo_aprobacioncompactospdf uo_aprobacioncompactospdf

forward prototypes
public function long maximo (integer ai_cliente, integer ai_planta, integer ai_tipo, long al_numero, integer ai_especie, date ad_fecha, transaction at_transaccion)
public function boolean grabaimagen (datawindow adw, long fila, transaction at_transaccion, string as_archivo)
public function blob seleccionadocto (datawindow adw, long fila, ref string as_file_tmp, transaction at_transaccion)
end prototypes

public function long maximo (integer ai_cliente, integer ai_planta, integer ai_tipo, long al_numero, integer ai_especie, date ad_fecha, transaction at_transaccion);Long	ll_Retorno

ll_retorno	=	1

Return ll_Retorno
end function

public function boolean grabaimagen (datawindow adw, long fila, transaction at_transaccion, string as_archivo);Boolean	lb_Retorno = True
Long		ll_File, capr_numero, orpr_numero, plde_codigo
Blob 		lbl_data, lbl_temp
Date		ld_Fecha
Integer	orpr_tipord, clie_codigo

clie_codigo	=	adw.Object.clie_codigo[fila]
plde_codigo	=	adw.Object.plde_codigo[fila]
orpr_tipord	=	adw.Object.orpr_tipord[fila]
orpr_numero	=	adw.Object.orpr_numero[fila]
capr_numero	=	adw.Object.capr_numero[fila]

ll_File = FileOpen(as_archivo, StreamMode!)

Do While FileRead(ll_file, lbl_temp) > 0
	lbl_data += lbl_temp
Loop

FileClose(ll_file)

If ll_File = 1 Then
	FileRead(ll_file, lbl_data)
	FileClose(ll_file)
	at_Transaccion.AutoCommit = True
	
	UpDateBlob dbo.spro_ordenprocdeta_cajasprod 
			Set capr_cmppdf  = :lbl_data 
			Where  clie_codigo	= :clie_codigo
				And plde_codigo	= :plde_codigo
				And orpr_tipord	= :orpr_tipord
				And orpr_numero	= :orpr_numero
				And capr_numero	= :capr_numero
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

public function blob seleccionadocto (datawindow adw, long fila, ref string as_file_tmp, transaction at_transaccion);Boolean	lb_Retorno = True
Long		ll_File, capr_numero, orpr_numero, plde_codigo
Blob 		lblob_file
Date		ld_Fecha
Integer	orpr_tipord, clie_codigo

SetNull(lblob_file)

clie_codigo	=	adw.Object.clie_codigo[fila]
plde_codigo	=	adw.Object.plde_codigo[fila]
orpr_tipord	=	adw.Object.orpr_tipord[fila]
orpr_numero	=	adw.Object.orpr_numero[fila]
capr_numero	=	adw.Object.capr_numero[fila]

as_file_tmp	=	String(adw.Object.orpr_numero[fila]) + '.' + String(adw.Object.capr_numero[fila]) + '.pdf'

SelectBlob capr_cmppdf
	Into :lblob_file
	From dbo.spro_ordenprocdeta_cajasprod
			Where  clie_codigo	= :clie_codigo
				And plde_codigo	= :plde_codigo
				And orpr_tipord	= :orpr_tipord
				And orpr_numero	= :orpr_numero
				And capr_numero	= :capr_numero
			Using at_Transaccion;

If at_transaccion.Sqlcode = -1 OR at_transaccion.Sqlcode = 100 Then
	MessageBox( "Atención", "Ha ocurrido el error: ~n~r" + at_transaccion.SqlErrText ) 
	Return lblob_file
End If

Return lblob_file
end function

on uo_aprobacioncompactospdf.create
call super::create
end on

on uo_aprobacioncompactospdf.destroy
call super::destroy
end on


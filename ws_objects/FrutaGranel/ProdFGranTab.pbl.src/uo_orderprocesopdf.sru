$PBExportHeader$uo_orderprocesopdf.sru
forward
global type uo_orderprocesopdf from uo_admdoctos
end type
end forward

global type uo_orderprocesopdf from uo_admdoctos
end type
global uo_orderprocesopdf uo_orderprocesopdf

forward prototypes
public function boolean grabaimagen (datawindow adw, long fila, transaction at_transaccion, string as_archivo)
public function blob seleccionadocto (datawindow adw, long fila, ref string as_file_tmp, transaction at_transaccion)
public function long maximo (integer ai_cliente, integer ai_planta, integer ai_tipo, long al_numero, transaction at_transaccion)
end prototypes

public function boolean grabaimagen (datawindow adw, long fila, transaction at_transaccion, string as_archivo);Boolean	lb_Retorno = True
Long		ll_File, ll_numero, ll_Secuencia
Blob 		lbl_data, lbl_temp
Integer	li_cliente, li_Planta, li_Tipo

li_cliente		=	adw.Object.clie_codigo[Fila]
li_Planta		=	adw.Object.plde_codigo[Fila]
li_tipo			=	adw.Object.orpr_tipord[Fila]
ll_numero		=	adw.Object.orpr_numero[Fila]
ll_Secuencia	=	adw.Object.orpr_secuen[Fila]

ll_File = FileOpen(as_archivo, StreamMode!)

Do While FileRead(ll_file, lbl_temp) > 0
	lbl_data += lbl_temp
Loop

FileClose(ll_file)

If ll_File = 1 Then
	FileRead(ll_file, lbl_data)
	FileClose(ll_file)
	at_Transaccion.AutoCommit = True
	
	UpDateBlob dba.ordenprocesopdf 
			Set orpr_imagen  = :lbl_data 
			Where orpr_numero 	= :ll_numero
				And clie_codigo		= :li_cliente
				And plde_codigo	= :li_Planta
				And orpr_tipord		= :li_Tipo
				And orpr_secuen	= :ll_Secuencia
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
Long		ll_Numero, ll_Secuencia
Integer	li_Cliente, li_Tipo, li_Planta

SetNull(lblob_file)

as_file_tmp	=	adw.Object.orpr_archiv[Fila]
li_cliente		=	adw.Object.clie_codigo[Fila]
li_Planta		=	adw.Object.plde_codigo[Fila]
li_tipo			=	adw.Object.orpr_tipord[Fila]
ll_numero		=	adw.Object.orpr_numero[Fila]
ll_Secuencia	=	adw.Object.orpr_secuen[Fila]

SelectBlob orpr_imagen
	Into :lblob_file
	From dba.ordenprocesopdf
	Where orpr_numero 	= :ll_numero
		And clie_codigo		= :li_cliente
		And plde_codigo	= :li_Planta
		And orpr_tipord		= :li_Tipo
		And orpr_secuen	= :ll_Secuencia
	Using at_transaccion;

If at_transaccion.Sqlcode = -1 OR at_transaccion.Sqlcode = 100 Then
	MessageBox( "Atención", "Ha ocurrido el error: ~n~r" + at_transaccion.SqlErrText ) 
	Return lblob_file
End If

Return lblob_file
end function

public function long maximo (integer ai_cliente, integer ai_planta, integer ai_tipo, long al_numero, transaction at_transaccion);Long	ll_Retorno

Select IsNull(Max(orpr_secuen), 0)
	Into :ll_Retorno
    From dba.ordenprocesopdf
    Where clie_codigo		= :ai_Cliente
		And plde_codigo	= :ai_planta
		And orpr_tipord		= :ai_Tipo
		And orpr_numero	= :al_numero
Using	at_transaccion;

IF At_Transaccion.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Transaccion,"Lectura de Tabla de Imagenes de Reclamos")
	ll_Retorno = -1
ELSEIF sqlca.SQLCode = 100 THEN	
	ll_Retorno = -1
END IF

Return ll_Retorno
end function

on uo_orderprocesopdf.create
call super::create
end on

on uo_orderprocesopdf.destroy
call super::destroy
end on


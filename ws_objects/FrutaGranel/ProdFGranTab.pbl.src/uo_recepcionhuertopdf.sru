$PBExportHeader$uo_recepcionhuertopdf.sru
forward
global type uo_recepcionhuertopdf from uo_admdoctos
end type
end forward

global type uo_recepcionhuertopdf from uo_admdoctos
end type
global uo_recepcionhuertopdf uo_recepcionhuertopdf

forward prototypes
public function long maximo (integer ai_cliente, integer ai_planta, integer ai_tipo, long al_numero, integer ai_especie, date ad_fecha, transaction at_transaccion)
public function boolean grabaimagen (datawindow adw, long fila, transaction at_transaccion, string as_archivo)
public function blob seleccionadocto (datawindow adw, long fila, ref string as_file_tmp, transaction at_transaccion)
end prototypes

public function long maximo (integer ai_cliente, integer ai_planta, integer ai_tipo, long al_numero, integer ai_especie, date ad_fecha, transaction at_transaccion);Long	ll_Retorno

Select IsNull(Max(rehu_secuen), 0)
	Into :ll_Retorno
    From dbo.spro_recepcionhuertopdf
    Where clie_codigo		= :ai_Cliente
		And plde_codigo	= :ai_planta
		And tpmv_codigo	= :ai_Tipo
		And mfge_numero	= :al_numero
		And espe_codigo 	= :ai_especie
		And mfge_fecmov	= :ad_fecha
Using	at_transaccion;

IF At_Transaccion.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Transaccion,"Lectura de Tabla de Imagenes de Reclamos")
	ll_Retorno = -1
ELSEIF sqlca.SQLCode = 100 THEN	
	ll_Retorno = -1
END IF

Return ll_Retorno
end function

public function boolean grabaimagen (datawindow adw, long fila, transaction at_transaccion, string as_archivo);Boolean	lb_Retorno = True
Long		ll_File, ll_numero, ll_Productor, ll_Secuencia
Blob 		lbl_data, lbl_temp
Date		ld_Fecha
Integer	li_cliente, li_especie, li_Planta, li_Tipo

ll_numero	=	adw.Object.mfge_numero[Fila]
li_cliente		=	adw.Object.clie_codigo[Fila]
li_especie	=	adw.Object.espe_codigo[Fila]
li_Planta		=	adw.Object.plde_codigo[Fila]
li_tipo			=	adw.Object.tpmv_codigo[Fila]
ll_Productor	=	adw.Object.prod_codigo[Fila]
ld_Fecha		=	adw.Object.mfge_fecmov[Fila]
ll_Secuencia	=	adw.Object.rehu_secuen[Fila]

ll_File = FileOpen(as_archivo, StreamMode!)

Do While FileRead(ll_file, lbl_temp) > 0
	lbl_data += lbl_temp
Loop

FileClose(ll_file)

If ll_File = 1 Then
	FileRead(ll_file, lbl_data)
	FileClose(ll_file)
	at_Transaccion.AutoCommit = True
	
	UpDateBlob dbo.spro_recepcionhuertopdf 
			Set rehu_doctos  = :lbl_data 
			Where mfge_numero = :ll_numero
				And clie_codigo		= :li_cliente
				And espe_codigo	= :li_especie
				And plde_codigo	= :li_Planta
				And tpmv_codigo	= :li_Tipo
				And prod_codigo	= :ll_Productor
				And mfge_fecmov	= :ld_Fecha
				And rehu_secuen	= :ll_Secuencia
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
Date		ld_Fecha
Long		ll_Numero, ll_Secuencia, ll_Productor
Integer	li_Cliente, li_Especie, li_Tipo, li_Planta

SetNull(lblob_file)

as_file_tmp	=	adw.Object.rehu_archiv[Fila]
ll_numero	=	adw.Object.mfge_numero[Fila]
li_cliente		=	adw.Object.clie_codigo[Fila]
li_especie	=	adw.Object.espe_codigo[Fila]
li_Planta		=	adw.Object.plde_codigo[Fila]
li_tipo			=	adw.Object.tpmv_codigo[Fila]
ll_Productor	=	adw.Object.prod_codigo[Fila]
ld_Fecha		=	adw.Object.mfge_fecmov[Fila]
ll_Secuencia	=	adw.Object.rehu_secuen[Fila]

SelectBlob rehu_doctos
	Into :lblob_file
	From dbo.spro_recepcionhuertopdf
	Where mfge_numero = :ll_numero
		And clie_codigo		= :li_cliente
		And espe_codigo	= :li_especie
		And plde_codigo	= :li_Planta
		And tpmv_codigo	= :li_Tipo
		And prod_codigo	= :ll_Productor
		And mfge_fecmov	= :ld_Fecha
		And rehu_secuen	= :ll_Secuencia
	Using at_transaccion;

If at_transaccion.Sqlcode = -1 OR at_transaccion.Sqlcode = 100 Then
	MessageBox( "Atención", "Ha ocurrido el error: ~n~r" + at_transaccion.SqlErrText ) 
	Return lblob_file
End If

Return lblob_file
end function

on uo_recepcionhuertopdf.create
call super::create
end on

on uo_recepcionhuertopdf.destroy
call super::destroy
end on


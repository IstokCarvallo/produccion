$PBExportHeader$uo_controlfrio.sru
forward
global type uo_controlfrio from nonvisualobject
end type
end forward

global type uo_controlfrio from nonvisualobject
end type
global uo_controlfrio uo_controlfrio

type variables
uo_CamarasBode	iuo_Camaras
end variables

forward prototypes
public function integer maxima (long planta, long camara, datetime fecha, transaction at_transaccion)
public function integer cuentadetalle (long planta, long camara, datetime fecha, integer sec, transaction at_transaccion)
public function boolean agregar (long planta, long camara, datetime fecha, integer sec, transaction at_transaccion)
end prototypes

public function integer maxima (long planta, long camara, datetime fecha, transaction at_transaccion);Integer	li_Retorno 

    Select IsNull(Max(esca_secuen), 0)
	 	Into	:li_Retorno 
	 	From dbo.estibacamaraenca
       Where :Planta in(-1, plde_codigo)
         And :Camara in(-1, cama_codigo)
         And DateDiff(dd, esca_fecpro, :Fecha) = 0
	Using at_transaccion;
	
If at_Transaccion.SQLCode = -1 Then
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Estiba")
	li_Retorno	=	0
ElseIf at_Transaccion.SQLCode = 100 Then
	li_Retorno	=	0
End If

Return li_Retorno
			
			
end function

public function integer cuentadetalle (long planta, long camara, datetime fecha, integer sec, transaction at_transaccion);Integer	li_Retorno 

    Select Count(paen_numero)
	 	Into :li_Retorno 
        From dbo.estibacamaradeta
       Where :Planta in(-1, plde_codigo)
         And :Camara in(-1, cama_codigo)
         And DateDiff(dd, esca_fecpro, :Fecha) = 0
		And :Sec in (-1, esca_secuen)
         And esca_activo = 0
	Using at_transaccion;
	
If at_Transaccion.SQLCode = -1 Then
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Estibas")
	li_Retorno	=	0
ElseIf at_Transaccion.SQLCode = 100 Then
	li_Retorno	=	0
End If

Return li_Retorno
			
			
end function

public function boolean agregar (long planta, long camara, datetime fecha, integer sec, transaction at_transaccion);Boolean	lb_Retorno = True
String		ls_Fecha
at_Transaccion.AutoCommit = False

ls_Fecha	=	String(Fecha, 'yyyymmdd')
	
If iuo_Camaras.Existe(Planta, Camara, False, Sqlca) Then
	Insert Into dbo.EstibaCamaraEnca(plde_codigo, cama_codigo, esca_fecpro, esca_secuen, esca_estado, emba_tipofr, 
												cama_capaci, cama_tipoca, esca_nropro, esca_fecest, usua_codigo, esca_tipcam)
		Values (:iuo_Camaras.Planta, :iuo_Camaras.Camara, :ls_Fecha, :Sec, 0, 0, :iuo_Camaras.Capacidad, 1, Null, Null, Null, 0)
		Using at_Transaccion;
		
	If at_Transaccion.SqlCode = -1 Then
		Rollback;
		lb_Retorno = False
	Else
		Commit;
	End If
Else
	lb_Retorno = False
End If

at_Transaccion.AutoCommit = True

Return lb_Retorno
end function

on uo_controlfrio.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_controlfrio.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event constructor;iuo_Camaras	=	Create uo_CamarasBode
end event


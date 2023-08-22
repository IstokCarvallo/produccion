$PBExportHeader$w_mant_deta_spro_detencionlineadeta.srw
forward
global type w_mant_deta_spro_detencionlineadeta from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_spro_detencionlineadeta from w_mant_detalle_csd
integer width = 2606
integer height = 1780
string title = "JUSTIFICACION TIEMPO DETENCION"
end type
global w_mant_deta_spro_detencionlineadeta w_mant_deta_spro_detencionlineadeta

type variables
DataWindowChild idwc_linea, idwc_detencion,idwc_especies
end variables

forward prototypes
public function boolean ExisteResumen ()
public function boolean existecoddet (string as_codigo)
public function boolean validaminutos (string as_valor)
public function boolean duplicado (string campo, integer tipo)
end prototypes

public function boolean ExisteResumen (); 
// SELECT   deli_nombre  
//    INTO  :ls_nombre 
//    FROM  dba.spro_motivodetenlinea  
//    WHERE area_codigo = :istr_Mant.Argumento[1]
//    AND	 deli_coddet = :as_codigo;
//
//	IF sqlca.SQLCode = -1 THEN
//			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla MOTIVODETENLINEA" )
//			RETURN TRUE
//	ELSEIF sqlca.SQLCode = 0 THEN
//			dw_1.SetItem(il_fila,"deli_coddet",as_codigo)
//			dw_1.SetItem(il_fila,"spro_motivodetenlinea_deli_nombre",ls_nombre)			
//			dw_1.SetFocus()
//			RETURN FALSE
//	ELSE
//		  MessageBox("Atención", "Código Detención No ha sido Ingresado Ingrese otro código.",Exclamation!, OK!)
		  RETURN TRUE
//	END IF		

end function

public function boolean existecoddet (string as_codigo);String	ls_nombre
 
 SELECT   deli_nombre  
    INTO  :ls_nombre 
    FROM  dba.spro_motivodetenlinea  
    WHERE area_codigo = :istr_Mant.Argumento[1]
    AND	 deli_coddet = :as_codigo;

	IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla MOTIVODETENLINEA" )
			RETURN TRUE
	ELSEIF sqlca.SQLCode = 0 THEN
			dw_1.SetItem(il_fila,"deli_coddet",as_codigo)			
			dw_1.SetFocus()
			RETURN FALSE
	ELSE
		  MessageBox("Atención", "Código Detención No ha sido Ingresado Ingrese otro código.",Exclamation!, OK!)
		  RETURN TRUE
	END IF		

end function

public function boolean validaminutos (string as_valor);Boolean	lb_Retorno
Integer	li_Planta, li_Linea, li_Especie, li_Cantidad,li_Area,li_Minutos,li_fila, li_Turno
Date  	ldt_Fecha, ldt_FechaNula
String   ls_fecha, ls_hora

li_Cantidad		= 0
li_Area			= Integer(istr_Mant.Argumento[1])
li_Planta		= gstr_ParamPlanta.CodigoPlanta
li_Especie		= Integer(istr_mant.argumento[3])
li_Linea			= Integer(istr_mant.argumento[4])
ldt_fecha		= Date(istr_mant.Argumento[5])	
li_Turno			= Integer(istr_Mant.Argumento[6])

SELECT	rdla_minuto
	INTO	:li_Minutos
	FROM	dba.spro_detencionlinearesu
	WHERE	area_codigo =  :li_Area
	AND	plde_codigo	=	:li_Planta
	AND	line_codigo	=	:li_linea
	AND	espe_codigo	=	:li_Especie
	AND	edla_fecpro	=	:ldt_Fecha 
	AND   rdla_turno	=	:li_Turno;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_detencionlinearesu ")
	RETURN TRUE
ELSEIF sqlca.SQLCode = 0 THEN
		FOR li_fila= 1 TO dw_1.RowCount()-1
			li_Cantidad = li_cantidad + Integer(dw_1.Object.ddla_minuto[li_fila])
		NEXT
		
		li_cantidad = li_cantidad + Integer(as_valor)
		
		IF li_cantidad > li_Minutos THEN
			MessageBox("Atención","Los Minutos Justificados Son Mayores" + &
							"~r~ra Los de Tiempo de Detención",Exclamation!)	
			Return True
//		ELSE				
//			li_Cantidad = li_minutos - li_cantidad
//			MessageBox("Atención","Faltan Justificar " + String(li_cantidad) +" Minutos",Exclamation!)				
		END IF
ELSE
		MessageBox("Atención","Area No Tiene Tienes Minutos a Justificar",Exclamation!)	
		RETURN TRUE
END IF	
Return False 
end function

public function boolean duplicado (string campo, integer tipo);Long		ll_fila
String	ls_codigo

ls_codigo	= String(dw_1.GetItemNumber(il_fila,"deli_coddet"))

CHOOSE CASE tipo
	CASE 1
		ls_codigo	= campo
END CHOOSE

ll_fila	= dw_1.Find("deli_coddet = " + ls_codigo, &
							1, dw_1.RowCount())

IF ll_fila > 0 AND ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresada anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

on w_mant_deta_spro_detencionlineadeta.create
call super::create
end on

on w_mant_deta_spro_detencionlineadeta.destroy
call super::destroy
end on

event ue_recuperadatos();call super::ue_recuperadatos;ias_campo[1] = String(dw_1.GetItemNumber(il_fila,"deli_coddet"))
ias_campo[2] = String(dw_1.GetItemNumber(il_fila,"ddla_minuto"))
ias_campo[3] = dw_1.GetItemString(il_fila,"ddla_observ")

dw_1.SetItem(il_fila, "area_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "plde_codigo", gstr_paramplanta.codigoPlanta)
dw_1.SetItem(il_fila, "line_codigo", Integer(istr_mant.argumento[4]))
dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[3]))
dw_1.SetItem(il_fila, "edla_fecpro", Date(istr_mant.argumento[5]))
dw_1.SetItem(il_fila, "rdla_turno",  Integer(istr_Mant.Argumento[6]))

IF istr_mant.agrega = False and istr_mant.borra = False THEN
	dw_1.settaborder("area_codigo",0)
	dw_1.settaborder("plde_codigo",0)
	dw_1.settaborder("line_codigo",0)
	dw_1.settaborder("espe_codigo",0)
	dw_1.settaborder("edla_fecpro",0)
	dw_1.Object.rdla_turno.Protect	=	1
END IF
end event

event ue_deshace();call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "deli_coddet", ias_campo[1])
	dw_1.SetItem(il_fila, "ddla_minuto", ias_campo[2])
	dw_1.SetItem(il_fila, "ddla_observ", ias_campo[3])
END IF






end event

event ue_antesguardar();Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.GetItemNumber(il_fila, "deli_coddet")) OR dw_1.GetItemNumber(il_fila, "deli_coddet") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Detención línea"
	ls_colu[li_cont]	= "deli_coddet"
END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "rdla_turno")) OR dw_1.GetItemNumber(il_fila, "rdla_turno") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nTurno"
	ls_colu[li_cont]	= "rlda_turno"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event open;x	= 100
y	= 450

istr_mant = Message.PowerObjectParm

//linea
dw_1.GetChild("espe_codigo", idwc_especies)
idwc_especies.SetTransObject(SqlCa)
//idwc_especies.Retrieve(gstr_parempresa.empr_codexp) 
idwc_especies.Retrieve() 

dw_1.GetChild("line_codigo", idwc_linea)
idwc_linea.SetTransObject(SqlCa)
IF idwc_linea.Retrieve(0) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
END IF

dw_1.GetChild("deli_coddet", idwc_detencion)
idwc_detencion.SetTransObject(SqlCa)
IF idwc_detencion.Retrieve(Integer(istr_Mant.Argumento[1])) = 0 THEN
	MessageBox("Atención","Falta Registrar Motivis Detención Líneas")
END IF

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)

This.TriggerEvent("ue_recuperadatos")


end event

event ue_nuevo();call super::ue_nuevo;	
dw_1.SetItem(il_fila, "area_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "plde_codigo", gstr_paramplanta.codigoPlanta)
dw_1.SetItem(il_fila, "line_codigo", Integer(istr_mant.argumento[4]))
dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[3]))
dw_1.SetItem(il_fila, "edla_fecpro", Date(istr_mant.argumento[5]))
dw_1.SetItem(il_fila, "rdla_turno",  Integer(istr_Mant.Argumento[6]))
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_spro_detencionlineadeta
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_spro_detencionlineadeta
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_spro_detencionlineadeta
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_spro_detencionlineadeta
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_spro_detencionlineadeta
integer x = 2277
integer y = 344
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_spro_detencionlineadeta
integer x = 2277
integer y = 128
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_spro_detencionlineadeta
integer x = 2277
integer y = 560
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_spro_detencionlineadeta
integer x = 87
integer y = 52
integer width = 1883
integer height = 1576
string dataobject = "dw_mant_spro_detencionlineadeta"
end type

event dw_1::itemchanged;call super::itemchanged;String  ls_columna, ls_Null

SetNull(ls_Null)
ls_columna = dwo.Name

CHOOSE CASE ls_columna
		
	CASE "deli_coddet"
		IF EXisteCodDet(data) = True THEN
			This.SetItem(il_fila, ls_columna, Integer(ias_campo[1]))
			RETURN 1
		ELSEIF Duplicado(data, 1) THEN
			This.SetItem(il_fila, ls_columna, Integer(ias_campo[1]))
			RETURN 1
		END IF
		
	CASE "ddla_minuto"
		IF Integer(data) < 0 OR Integer(data) > 999 THEN
			MessageBox("Error de Consistencia","El valor de minuto no es permitido")
			This.SetItem(row, "ddla_minuto",Integer(ls_Null))
			RETURN 1
		ELSEIF validaMinutos(data) THEN
			This.SetItem(il_fila, ls_columna, Integer(ias_campo[2]))
			RETURN 1
		END IF
END CHOOSE

end event


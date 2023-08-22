$PBExportHeader$w_mant_deta_romanas.srw
forward
global type w_mant_deta_romanas from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_romanas from w_mant_detalle_csd
integer width = 2747
integer height = 1768
string title = "EXPORTADORES"
end type
global w_mant_deta_romanas w_mant_deta_romanas

type variables
DataWindowChild  idwc_Planta
end variables

forward prototypes
public function boolean duplicado (string campo)
end prototypes

public function boolean duplicado (string campo);Long		ll_fila
		

ll_fila	= dw_1.Find("crpl_equcon = '" + campo + "'", 1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
return true
end function

on w_mant_deta_romanas.create
call super::create
end on

on w_mant_deta_romanas.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1]	= String(dw_1.GetItemNumber(il_fila,"plde_codigo"))
ias_campo[2]	= dw_1.GetItemString(il_fila,"crpl_equcon")
ias_campo[3]	= dw_1.GetItemString(il_fila,"crpl_puerta")
ias_campo[4]	= String(dw_1.GetItemNumber(il_fila,"crpl_baudio"))
ias_campo[5]	= dw_1.GetItemString(il_fila,"crpl_parida")
ias_campo[6]   = String(dw_1.GetItemNumber(il_fila,"crpl_data"))
ias_campo[7]	= String(dw_1.GetItemNumber(il_fila,"crpl_parada"))
ias_campo[8]	= String(dw_1.GetItemNumber(il_fila,"crpl_larlec"))
ias_campo[9]   = String(dw_1.GetItemNumber(il_fila,"crpl_larcad"))
ias_campo[10]	= dw_1.GetItemString(il_fila,"crpl_cadini")
ias_campo[11]	= String(dw_1.GetItemNumber(il_fila,"crpl_nrodec"))
ias_campo[12]	= String(dw_1.GetItemNumber(il_fila,"crpl_ishico"))


IF istr_mant.Agrega THEN
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.Argumento[1]))
	ias_campo[1]	=	istr_mant.Argumento[1]
END IF	

end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "plde_codigo", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "crpl_equcon", ias_campo[2])
	dw_1.SetItem(il_fila, "crpl_puerta", ias_campo[3])
	dw_1.SetItem(il_fila, "crpl_baudio", Integer(ias_campo[4]))
	dw_1.SetItem(il_fila, "crpl_parida", ias_campo[5])
	
	dw_1.SetItem(il_fila, "crpl_data", Integer(ias_campo[6]))
	dw_1.SetItem(il_fila, "crpl_parada", Integer(ias_campo[7]))
	dw_1.SetItem(il_fila, "crpl_larlec", Integer(ias_campo[8]))
	dw_1.SetItem(il_fila, "crpl_larcad", Integer(ias_campo[9]))
	dw_1.SetItem(il_fila, "crpl_cadini", ias_campo[10])	
	dw_1.SetItem(il_fila, "crpl_nrodec", Integer(ias_campo[11]))
	dw_1.SetItem(il_fila, "crpl_ishico", Integer(ias_campo[12]))
END IF


end event

event ue_antesguardar;Integer	li_cont,li_fila, li_ordena
String	ls_mensaje, ls_colu[]

IF IsNull(dw_1.GetItemNumber(il_fila, "plde_codigo")) OR dw_1.GetItemNumber(il_fila, "plde_codigo") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Planta"
	ls_colu[li_cont]	= "plde_codigo"
END IF

IF IsNull(dw_1.GetItemString(il_fila, "crpl_equcon")) OR dw_1.GetItemString(il_fila, "crpl_equcon") = "" THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nNombre Equipo"
	ls_colu[li_cont]	= "crpl_equcon"
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

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

dw_1.GetChild("plde_codigo", idwc_Planta)
idwc_Planta.SetTransObject(SqlCa)

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)
end event

event ue_nuevo;call super::ue_nuevo;Long	ll_fila

dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.Argumento[1]))
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_romanas
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_romanas
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_romanas
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_romanas
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_romanas
integer x = 2158
integer y = 480
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_romanas
integer x = 2158
integer y = 264
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_romanas
integer x = 2158
integer y = 696
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_romanas
integer x = 78
integer y = 152
integer width = 1833
integer height = 1188
string dataobject = "dw_mant_romamas"
end type

event dw_1::itemchanged;String	ls_null

SetNull(ls_null)

CHOOSE CASE dwo.name
		
	CASE "crpl_equcon"
		IF Duplicado(data) THEN
			dw_1.SetItem(il_fila, "crpl_equcon",ls_null)
			RETURN 1
		END IF
END CHOOSE
end event


$PBExportHeader$w_mant_deta_ciudades.srw
forward
global type w_mant_deta_ciudades from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_ciudades from w_mant_detalle_csd
integer width = 2496
integer height = 1060
string title = "CIUDADES"
end type
global w_mant_deta_ciudades w_mant_deta_ciudades

type variables
DataWindowChild	idwc_provincias
end variables

forward prototypes
public function boolean duplicado (string campo, integer tipo)
end prototypes

public function boolean duplicado (string campo, integer tipo);Long		ll_fila
String	ciud_codigo

ciud_codigo	= String(dw_1.GetItemNumber(il_fila,"ciud_codigo"))

CHOOSE CASE tipo
	CASE 1
		ciud_codigo	= campo
END CHOOSE

ll_fila	= istr_mant.dw.Find("ciud_codigo = " + ciud_codigo,1, istr_mant.dw.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Código ya fue ingresada anteriormente",Information!, Ok!)
	RETURN True
ELSE
	istr_mant.argumento[1]	=	ciud_codigo
	RETURN False
END IF
end function

on w_mant_deta_ciudades.create
call super::create
end on

on w_mant_deta_ciudades.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = String(dw_1.GetItemNumber(il_fila,"ciud_codigo"))
ias_campo[2] = String(dw_1.GetItemNumber(il_fila,"regi_codigo"))
ias_campo[3] = String(dw_1.GetItemNumber(il_fila,"prov_codigo"))
ias_campo[4] = dw_1.GetItemString(il_fila,"ciud_nombre")

IF Not istr_mant.Agrega And Not istr_mant.Borra THEN
	dw_1.Object.ciud_codigo.BackGround.Color = 553648127
	dw_1.Object.ciud_codigo.Color 				= RGB(255,255,255)
	
	dw_1.Object.regi_codigo.BackGround.Color 				= 553648127
	
	dw_1.Object.nomprov.text						=	istr_mant.argumento[3]
ELSE
	dw_1.Object.regi_codigo[il_fila]	=	Integer(istr_mant.argumento[1])
	dw_1.Object.prov_codigo[il_fila]	=	Integer(istr_mant.argumento[2])
	dw_1.Object.nomprov.text			=	istr_mant.argumento[3]
END IF
end event

event ue_deshace();call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "ciud_codigo", Long(ias_campo[1]))
	dw_1.SetItem(il_fila, "regi_codigo", Long(ias_campo[2]))
	dw_1.SetItem(il_fila, "prov_codigo", Long(ias_campo[3]))
	dw_1.SetItem(il_fila, "ciud_nombre", ias_campo[4])	
END IF
end event

event ue_antesguardar();Integer	li_cont
String	ls_mensaje, ls_mens[], ls_colu[]

IF Isnull(dw_1.GetItemNumber(il_fila, "ciud_codigo")) OR dw_1.GetItemNumber(il_fila, "ciud_codigo") = 0 THEN
	li_cont ++
	ls_mensaje = 	ls_mensaje + "~nCódigo de Ciudad"
	ls_colu[li_cont] = "ciud_codigo"
END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "prov_codigo")) OR dw_1.GetItemNumber(il_fila, "prov_codigo") = 0 THEN
	li_cont ++
	ls_mensaje = 	ls_mensaje + "~nCódigo de Provincia"
	ls_colu[li_cont] = "prov_codigo"
END IF
	
IF Isnull(dw_1.GetItemString(il_fila, "ciud_nombre")) OR dw_1.GetItemString(il_fila, "ciud_nombre") = "" THEN
	li_cont ++
	ls_mensaje = 	ls_mensaje + "~nNombre de Ciudad"
	ls_colu[li_cont] = "ciud_nombre"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.Object.regi_codigo[il_fila]	=	Integer(istr_mant.argumento[1])
dw_1.Object.prov_codigo[il_fila]	=	Integer(istr_mant.argumento[2])
dw_1.Object.nomprov.text			=	istr_mant.argumento[3]
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_ciudades
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_ciudades
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_ciudades
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_ciudades
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_ciudades
integer x = 2171
integer y = 460
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_ciudades
integer x = 2171
integer y = 244
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_ciudades
integer x = 2171
integer y = 676
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_ciudades
integer y = 68
integer width = 1806
integer height = 788
string dataobject = "dw_mant_ciudades"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna
Integer	li_null

SEtNull(li_null)

ls_columna = GetColumnName()

CHOOSE CASE ls_columna
	CASE "ciud_codigo"
		IF Duplicado(data, 1) THEN
			This.SetItem(il_fila, ls_columna, li_null)
			RETURN 1
		END IF
		
	CASE	"regi_codigo"
		istr_mant.argumento[2]	=	data
			dw_1.GetChild("prov_codigo", idwc_provincias)
			idwc_provincias.SetTransObject(sqlca)
			dw_1.SetTransObject(sqlca)
		IF (idwc_provincias.Retrieve(Integer(istr_mant.argumento[2])))	=	0 THEN
			MessageBox("Atención","No Existe Información Para Esta Región",Exclamation!)
			This.SetItem(il_fila, ls_columna, li_null)
			RETURN 1
		ELSE
			istr_mant.argumento[3]	=	data
		END IF	
		
	CASE	"prov_codigo"
		
END CHOOSE
end event


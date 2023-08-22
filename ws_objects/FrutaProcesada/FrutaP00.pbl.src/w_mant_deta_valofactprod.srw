$PBExportHeader$w_mant_deta_valofactprod.srw
$PBExportComments$Mantención Valores de Facturación por Productor.
forward
global type w_mant_deta_valofactprod from w_mant_detalle
end type
end forward

global type w_mant_deta_valofactprod from w_mant_detalle
integer width = 2651
integer height = 1328
end type
global w_mant_deta_valofactprod w_mant_deta_valofactprod

type variables
DataWindowChild	idwc_especie, idwc_variedad, idwc_calibre

Date		id_FechaAcceso
Time		it_HoraAcceso

end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor)
end prototypes

public function boolean duplicado (string as_columna, string as_valor);Long		ll_fila
String	ls_especie, ls_variedad, ls_calibre

ls_especie	= String(dw_1.GetItemNumber(il_fila,"espe_codigo"))
ls_variedad	= String(dw_1.GetItemNumber(il_fila,"vari_codigo"))
ls_calibre	= dw_1.GetItemString(il_fila,"vaca_calibr")

CHOOSE CASE as_columna
	CASE "espe_codigo"
		ls_especie	= as_valor
		
	CASE "vari_codigo"
		ls_variedad	= as_valor
				
	CASE "vaca_calibr"
		ls_calibre	= as_valor

END CHOOSE

ll_fila	= istr_mant.dw.Find("espe_codigo = " + ls_especie + " and " + &
							"vari_codigo = " + ls_variedad + " and " + &
							"vaca_calibr = '" + ls_calibre + "'", &	
							1, istr_mant.dw.RowCount())

IF ll_fila > 0 and ll_fila <> il_FilaAnc THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

on w_mant_deta_valofactprod.create
call super::create
end on

on w_mant_deta_valofactprod.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1]  = String(dw_1.GetItemNumber(il_fila, "espe_codigo"))
ias_campo[2]  = String(dw_1.GetItemNumber(il_fila, "vari_codigo"))
ias_campo[3]  = dw_1.GetItemString(il_fila, "vaca_calibr")
ias_campo[4]  = String(dw_1.GetItemDecimal(il_fila, "vafp_preuni"))

dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "prod_codigo", Long(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "prod_nombre", istr_mant.argumento[3])

dw_1.SetItemStatus(il_fila, "clie_codigo", Primary!, NotModified!)
dw_1.SetItemStatus(il_fila, "prod_codigo", Primary!, NotModified!)
dw_1.SetItemStatus(il_fila, "prod_nombre", Primary!, NotModified!)

IF istr_mant.agrega = False THEN
	idwc_variedad.Retrieve(Integer(istr_mant.Argumento[1]), Integer(ias_campo[1]))
	idwc_calibre.Retrieve(Integer(istr_mant.Argumento[1]), Integer(ias_campo[1]), Integer(ias_campo[2]))
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "prod_codigo", Long(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "prod_nombre", istr_mant.argumento[3])

dw_1.SetItemStatus(il_fila, "clie_codigo", Primary!, NotModified!)
dw_1.SetItemStatus(il_fila, "prod_codigo", Primary!, NotModified!)
dw_1.SetItemStatus(il_fila, "prod_nombre", Primary!, NotModified!)
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "espe_codigo", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "vari_codigo", Integer(ias_campo[2]))
	dw_1.SetItem(il_fila, "vaca_calibr", ias_campo[3])
	dw_1.SetItem(il_fila, "vafp_preuni", Dec(ias_campo[4]))
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]


IF Isnull(dw_1.GetItemNumber(il_fila, "espe_codigo")) OR dw_1.GetItemNumber(il_fila, "espe_codigo") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nEspecie "
	ls_colu[li_cont]	= "espe_codigo"
END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "vari_codigo")) OR dw_1.GetItemNumber(il_fila, "vari_codigo") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nVariedad "
	ls_colu[li_cont]	= "vari_codigo"
END IF

IF Isnull(dw_1.GetItemString(il_fila, "vaca_calibr")) OR dw_1.GetItemString(il_fila, "vaca_calibr") = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCalibre "
	ls_colu[li_cont]	= "vaca_calibr"
END IF

IF Isnull(dw_1.GetItemDecimal(il_fila, "vafp_preuni")) OR dw_1.GetItemDecimal(il_fila, "vafp_preuni") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nPrecio Unitario "
	ls_colu[li_cont]	= "vafp_preuni"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event open;/*
	Argumentos :
						[1]	=	Código de Cliente
						[2]	=	Código de Productor
						[3]	=	Nombre de Productor
						[4]	=	Código de Zona
*/
x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

dw_1.GetChild("espe_codigo", idwc_especie)
dw_1.GetChild("vari_codigo", idwc_variedad)
dw_1.GetChild("vaca_calibr", idwc_calibre)

idwc_especie.SetTransObject(sqlca)
idwc_variedad.SetTransObject(sqlca)
idwc_calibre.SetTransObject(sqlca)

idwc_especie.Retrieve(Integer(istr_mant.Argumento[1]))
idwc_variedad.Retrieve(Integer(istr_mant.Argumento[1]), gi_CodEspecie)
idwc_calibre.Retrieve(Integer(istr_mant.Argumento[1]), gi_CodEspecie, gi_CodVariedad)

dw_1.SetTransObject(sqlca)
//istr_mant.dw.ShareData(dw_1)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

end event

event closequery;call super::closequery;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_valofactprod
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_valofactprod
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_valofactprod
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_valofactprod
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_valofactprod
integer x = 2359
integer y = 384
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_valofactprod
integer x = 2359
integer y = 204
end type

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_valofactprod
integer x = 2359
integer y = 564
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_valofactprod
integer x = 37
integer y = 88
integer width = 2190
integer height = 1116
string dataobject = "dw_mant_valofactprod"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna
Integer	li_especie

ls_columna	=	dwo.Name

CHOOSE CASE ls_columna
	CASE "espe_codigo"
		IF Duplicado(ls_columna,data) THEN
			This.SetItem(row, ls_columna, Integer(ias_campo[1]))
			RETURN 1
		END IF
		idwc_variedad.Retrieve(Integer(istr_mant.Argumento[1]), Integer(data))
		idwc_calibre.Retrieve(Integer(istr_mant.Argumento[1]), Integer(data), 1)
		
	CASE "vari_codigo"
		IF Duplicado(ls_columna,data) THEN
			This.SetItem(row, ls_columna, Integer(ias_campo[2]))
			RETURN 1
		END IF
		
		This.Object.vari_nombre[row]	=	idwc_variedad.GetItemString(idwc_variedad.GetRow(),"vari_nombre")

		li_especie	=	This.Object.espe_codigo[row]
		
		idwc_calibre.Retrieve(Integer(istr_mant.Argumento[1]), li_especie, Integer(data))
		
	CASE "vaca_calibr"

		IF Duplicado(ls_columna,data) THEN
			This.SetItem(row, ls_columna, ias_campo[3])
			RETURN 1
		END IF
		
END CHOOSE
end event


$PBExportHeader$w_mant_deta_distribu_calibre.srw
$PBExportComments$Mantenedor de Daños por Especie.
forward
global type w_mant_deta_distribu_calibre from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_distribu_calibre from w_mant_detalle_csd
integer x = 123
integer y = 96
integer width = 2167
integer height = 1064
end type
global w_mant_deta_distribu_calibre w_mant_deta_distribu_calibre

type variables
DataWindowChild	 	idwc_especie, idwc_variedad

uo_calibre   iuo_calibre

end variables

forward prototypes
public function boolean duplicados (string campo, string valor)
end prototypes

public function boolean duplicados (string campo, string valor);Long		ll_fila, ll_Especie, ll_Variedad, ll_Secuencia

ll_Especie		=	dw_1.Object.espe_codigo[il_fila]
ll_Variedad		=	dw_1.Object.vari_codigo[il_fila]
ll_Secuencia	=	dw_1.Object.ccdc_secuen[il_fila]

CHOOSE CASE campo	
	CASE "espe_codigo"
		ll_Especie		=	Long(valor)
		
	CASE "vari_codigo"
		ll_Variedad		=	Long(valor)
		
	CASE "ccdc_ordena"
		ll_Secuencia	=	Long(valor)
	
END CHOOSE

ll_fila	= dw_1.Find("ccdc_ordena = " + valor,1, dw_1.RowCount())
							
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Atención","Registro ya fue ingresado",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

on w_mant_deta_distribu_calibre.create
call super::create
end on

on w_mant_deta_distribu_calibre.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;If Not istr_mant.agrega AND Not istr_mant.borra Then
	ias_campo[2]	= 	String(dw_1.Object.espe_codigo[il_Fila])
	ias_campo[3]	= 	String(dw_1.Object.vari_codigo[il_Fila])
	ias_campo[4]	= 	dw_1.Object.ccdc_calibr[il_Fila]
	ias_campo[5]	= 	dw_1.Object.ccdc_gramos[il_Fila]
	ias_campo[6]	= 	dw_1.Object.ccdc_milime[il_Fila]
	ias_campo[7]	= 	String(dw_1.Object.ccdc_secuen[il_Fila])
	ias_campo[8]	= 	String(dw_1.Object.ccdc_ordena[il_Fila])
ElseIf istr_mant.agrega Then
	dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "vari_codigo", Integer(istr_mant.argumento[3]))
	
	dw_1.SetItemStatus(il_fila, "espe_codigo", Primary!, NotModIfied!)
	dw_1.SetItemStatus(il_fila, "vari_codigo", Primary!, NotModIfied!)
End If
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "espe_codigo", Integer(ias_campo[2]))
	dw_1.SetItem(il_fila, "vari_codigo", Integer(ias_campo[3]))
	dw_1.Object.ccdc_calibr[il_Fila]	=	ias_campo[4]
	dw_1.Object.ccdc_gramos[il_Fila]	=	ias_campo[5]
	dw_1.Object.ccdc_milime[il_Fila]	=	ias_campo[6]
	dw_1.Object.ccdc_secuen[il_Fila]	=	Integer(ias_campo[7])
	dw_1.Object.ccdc_ordena[il_Fila]	=	Integer(ias_campo[8])	
END IF

end event

event ue_antesguardar;String	ls_mensaje, ls_colu[]
Integer	li_cont

IF Isnull(dw_1.Object.ccdc_calibr[il_fila]) OR trim(dw_1.Object.ccdc_calibr[il_fila]) = "" THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCalibre"
	ls_colu[li_cont]	= "ccdc_calibr"
END IF

IF Isnull(dw_1.Object.ccdc_ordena[il_fila]) OR dw_1.Object.ccdc_ordena[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nOrden"
	ls_colu[li_cont]	= "ccdc_ordena"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "vari_codigo", Integer(istr_mant.argumento[3]))




end event

event open;/*
	Argumentos :
	[2]	=	Código de especie
	[3]   =  Código de Variedad						
*/
Long ls_Null, li_cuenta

x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono
PostEvent("ue_recuperadatos")
istr_mant = Message.PowerObjectParm

dw_1.GetChild("espe_codigo", idwc_especie)
dw_1.GetChild("vari_codigo", idwc_variedad)
idwc_especie.SetTransObject(SQLCA)
idwc_variedad.SetTransObject(SQLCA)
idwc_especie.Retrieve()
IF idwc_variedad.Retrieve(Integer(istr_mant.Argumento[2]))	=	0 THEN  idwc_variedad.insertrow(0)

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)

iuo_calibre = create uo_calibre
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_distribu_calibre
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_distribu_calibre
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_distribu_calibre
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_distribu_calibre
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_distribu_calibre
integer x = 1742
integer y = 428
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_distribu_calibre
integer x = 1737
integer y = 212
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_distribu_calibre
integer x = 1737
integer y = 644
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_distribu_calibre
integer x = 96
integer y = 96
integer width = 1454
integer height = 792
string dataobject = "dw_mant_distribu_calibre"
boolean maxbox = true
end type

event dw_1::itemchanged;String   ls_Null

SetNull(ls_Null)

CHOOSE CASE dwo.Name	
	CASE "ccdc_ordena"
		IF Duplicados(dwo.Name, data) THEN
			This.SetItem(il_fila, "ccdc_ordena", Integer(ls_Null))
			RETURN 1
		END IF

END CHOOSE

end event

event dw_1::itemerror;call super::itemerror;RETURN 1
end event


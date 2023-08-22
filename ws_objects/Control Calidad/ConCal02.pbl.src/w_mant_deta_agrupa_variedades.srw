$PBExportHeader$w_mant_deta_agrupa_variedades.srw
$PBExportComments$Mantenedor de Daños por Especie.
forward
global type w_mant_deta_agrupa_variedades from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_agrupa_variedades from w_mant_detalle_csd
integer x = 123
integer y = 96
integer width = 2043
integer height = 1104
end type
global w_mant_deta_agrupa_variedades w_mant_deta_agrupa_variedades

type variables
DataWindowChild	 	idwc_especie, idwc_variedad

end variables

forward prototypes
public function boolean duplicados (string campo, string valor)
end prototypes

public function boolean duplicados (string campo, string valor);Long		ll_fila, ll_Especie, ll_Variedad

ll_Especie		=	dw_1.Object.espe_codigo[il_fila]
ll_Variedad		=	dw_1.Object.vari_codigo[il_fila]

CHOOSE CASE campo
	CASE "espe_codigo"
		ll_Especie		=	Long(valor)
		
	CASE "vari_codigo"
		ll_Variedad		=	Long(valor)

END CHOOSE

ll_fila	= dw_1.Find("vari_codigo = " + String(ll_Variedad), 1, dw_1.RowCount())
							
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Atención","Registro ya fue ingresado",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

on w_mant_deta_agrupa_variedades.create
call super::create
end on

on w_mant_deta_agrupa_variedades.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;IF istr_mant.agrega = False AND istr_mant.borra = False THEN
	ias_campo[1]	= 	String(dw_1.Object.espe_codigo[il_Fila])
	ias_campo[2]	= 	String(dw_1.Object.vari_codigo[il_Fila])
	ias_campo[3]	= 	dw_1.Object.ccdc_calibr[il_Fila]
	ias_campo[4]	= 	dw_1.Object.ccdc_gramos[il_Fila]
ELSEIF istr_mant.agrega = True THEN
	dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "agva_codigo", Integer(istr_mant.argumento[3]))
	dw_1.SetItem(il_fila, "agva_nombre", istr_mant.argumento[4])
	
	dw_1.SetItemStatus(il_fila, "espe_codigo", Primary!, NotModified!)
	dw_1.SetItemStatus(il_fila, "agva_codigo", Primary!, NotModified!)
	dw_1.SetItemStatus(il_fila, "agva_nombre", Primary!, NotModified!)
END IF
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "espe_codigo", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "vari_codigo", Integer(ias_campo[2]))
	dw_1.Object.agva_codigo[il_Fila]	=	Integer(ias_campo[3])
	dw_1.Object.agva_nombre[il_Fila]	=	ias_campo[4]
END IF
end event

event ue_antesguardar;String	ls_mensaje, ls_colu[]
Integer	li_cont

IF Isnull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nVariedad"
	ls_colu[li_cont]	= "vari_codigo"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "agva_codigo", Integer(istr_mant.argumento[3]))
dw_1.SetItem(il_fila, "agva_nombre", istr_mant.argumento[4])





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
idwc_especie.Retrieve()

idwc_variedad.SetTransObject(SQLCA)

IF idwc_variedad.Retrieve(Integer(istr_mant.Argumento[2]))	=	0 THEN 
	idwc_variedad.insertrow(0)
END IF

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_agrupa_variedades
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_agrupa_variedades
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_agrupa_variedades
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_agrupa_variedades
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_agrupa_variedades
integer x = 1742
integer y = 428
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_agrupa_variedades
integer x = 1742
integer y = 200
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_agrupa_variedades
integer x = 1737
integer y = 644
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_agrupa_variedades
integer x = 78
integer y = 96
integer width = 1582
integer height = 828
string dataobject = "dw_mant_mues_agrupavariedades"
boolean maxbox = true
end type

event dw_1::itemchanged;String   ls_Null, ls_Columna

SetNull(ls_Null)
ls_Columna = dwo.Name

CHOOSE CASE ls_Columna
	CASE "vari_codigo"
		IF Duplicados(ls_Columna, data) THEN
			This.SetItem(il_fila, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF

END CHOOSE

end event

event dw_1::itemerror;call super::itemerror;RETURN 1
end event


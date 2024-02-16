$PBExportHeader$w_mant_deta_facturprodgenerado.srw
$PBExportComments$Mantención Valores de Facturación por Productor.
forward
global type w_mant_deta_facturprodgenerado from w_mant_detalle
end type
end forward

global type w_mant_deta_facturprodgenerado from w_mant_detalle
integer width = 2665
integer height = 1760
end type
global w_mant_deta_facturprodgenerado w_mant_deta_facturprodgenerado

type variables
DataWindowChild	idwc_especie, idwc_variedad, idwc_calibre
end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor)
public function boolean existeembalaje (string ls_columna)
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

ll_fila	= dw_1.Find("espe_codigo = " + ls_especie + " and " + &
							"vari_codigo = " + ls_variedad + " and " + &
							"vaca_calibr = '" + ls_calibre + "'", &	
							1, istr_mant.dw.RowCount())

IF ll_fila > 0 and ll_fila <> il_filaAnc THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean existeembalaje (string ls_columna);String	ls_codigo, ls_nombre
Integer	li_cliente
Decimal	ll_pesone

li_cliente	= Integer(istr_mant.argumento[1])
ls_codigo	= ls_columna

SELECT	emb.emba_nombre, env.enva_pesone
	INTO 	:ls_nombre, :ll_pesone
	FROM	dbo.embalajesprod as emb, dbo.envases as env
	WHERE	emb.clie_codigo	= :li_cliente
	AND	emb.emba_codigo	= :ls_codigo
	AND	env.enva_tipoen	= emb.enva_tipoen
	AND	env.enva_codigo	= emb.enva_codigo;
		
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Embalajes")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Embalaje no Existe para este Cliente. Ingrese Otro.", Exclamation!, Ok!)
	RETURN False
ELSE
	dw_1.SetItem(il_fila, "enva_pesone", ll_pesone)
	RETURN True
END IF
end function

on w_mant_deta_facturprodgenerado.create
call super::create
end on

on w_mant_deta_facturprodgenerado.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1]  = String(dw_1.Object.espe_codigo[il_fila])
ias_campo[2]  = String(dw_1.Object.vari_codigo[il_fila])
ias_campo[3]  = dw_1.Object.vaca_calibr[il_fila]
ias_campo[4]  = dw_1.Object.emba_codigo[il_fila]
ias_campo[5]  = String(dw_1.Object.fade_cancaj[il_fila])
ias_campo[6]  = String(dw_1.Object.fade_cankls[il_fila])
ias_campo[7]  = String(dw_1.Object.fade_valous[il_fila])
ias_campo[8]  = String(dw_1.Object.fade_valope[il_fila])
ias_campo[9]  = String(dw_1.Object.fade_estado[il_fila])

IF istr_mant.argumento[15] = '2' THEN
	dw_1.SetItem(il_fila, "fade_estado", 2)
   dw_1.Object.fade_estado.Protect	=	1
ELSE
	dw_1.Object.fade_estado.Protect	=	0
END IF

dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "faen_fechaf", Date(istr_mant.argumento[3]))
dw_1.SetItem(il_fila, "prod_codigo", Long(istr_mant.argumento[4]))
dw_1.SetItem(il_fila, "zona_codigo", Integer(istr_mant.argumento[9]))
dw_1.SetItem(il_fila, "zona_nombre", istr_mant.argumento[10])

//dw_1.SetItemStatus(il_fila, "clie_codigo", Primary!, NotModified!)
//dw_1.SetItemStatus(il_fila, "prod_codigo", Primary!, NotModified!)
//dw_1.SetItemStatus(il_fila, "prod_nombre", Primary!, NotModified!)

IF istr_mant.agrega = False THEN
	idwc_variedad.Retrieve(Integer(ias_campo[1]))
	idwc_calibre.Retrieve(Integer(ias_campo[1]), Integer(ias_campo[2]))
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "faen_fechaf", Date(istr_mant.argumento[3]))
dw_1.SetItem(il_fila, "prod_codigo", Long(istr_mant.argumento[4]))
dw_1.SetItem(il_fila, "zona_codigo", Integer(istr_mant.argumento[9]))
dw_1.SetItem(il_fila, "zona_nombre", istr_mant.argumento[10])

//dw_1.SetItemStatus(il_fila, "clie_codigo", Primary!, NotModified!)
//dw_1.SetItemStatus(il_fila, "prod_codigo", Primary!, NotModified!)
//dw_1.SetItemStatus(il_fila, "prod_nombre", Primary!, NotModified!)
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "espe_codigo", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "vari_codigo", Integer(ias_campo[2]))
	dw_1.SetItem(il_fila, "vaca_calibr", ias_campo[3])
	dw_1.SetItem(il_fila, "emba_codigo", ias_campo[4])
	dw_1.SetItem(il_fila, "fade_cancaj", Integer(ias_campo[5]))	
	dw_1.SetItem(il_fila, "fade_cankls", Dec(ias_campo[6]))	
	dw_1.SetItem(il_fila, "fade_valous", Dec(ias_campo[7]))	
	dw_1.SetItem(il_fila, "fade_valope", Dec(ias_campo[8]))		
	dw_1.SetItem(il_fila, "fade_estado", Integer(ias_campo[9]))	
	
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

IF Isnull(dw_1.GetItemString(il_fila, "emba_codigo")) OR dw_1.GetItemString(il_fila, "emba_codigo") = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nEmbalaje "
	ls_colu[li_cont]	= "emba_codigo"
END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "fade_cancaj")) OR dw_1.GetItemNumber(il_fila, "fade_cancaj") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCajas "
	ls_colu[li_cont]	= "fade_cancaj"
END IF

IF Isnull(dw_1.GetItemDecimal(il_fila, "fade_cankls")) OR dw_1.GetItemDecimal(il_fila, "fade_cankls") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nKilos "
	ls_colu[li_cont]	= "fade_cankls"
END IF

IF Isnull(dw_1.GetItemDecimal(il_fila, "fade_valous")) OR dw_1.GetItemDecimal(il_fila, "fade_valous") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nValor Dólar "
	ls_colu[li_cont]	= "fade_valous"
END IF

IF Isnull(dw_1.GetItemDecimal(il_fila, "fade_valope")) OR dw_1.GetItemDecimal(il_fila, "fade_valope") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nValor Pesos "
	ls_colu[li_cont]	= "fade_valope"
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
						[2]	=	Código de Planta
						[3]	=	Mes de Proceso
						[4]	=	Código Productor
						[5]	=	Código Especie
						[6]	=	Código Variedad
						[8]	=	Nombre Productor
						[9]	=	Nombre Zona
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

idwc_especie.Retrieve()
idwc_variedad.Retrieve(Integer(istr_mant.Argumento[5]))
idwc_calibre.Retrieve(Integer(istr_mant.Argumento[5]), Integer(istr_mant.Argumento[6]))


end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_facturprodgenerado
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_facturprodgenerado
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_facturprodgenerado
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_facturprodgenerado
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_facturprodgenerado
integer x = 2245
integer y = 824
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_facturprodgenerado
integer x = 2245
integer y = 644
end type

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_facturprodgenerado
integer x = 2245
integer y = 1004
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_facturprodgenerado
integer x = 69
integer y = 124
integer width = 2098
integer height = 1448
string dataobject = "dw_mant_facturprodgenerado"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna
Integer	li_especie
Long		ll_null

SetNull(ll_null)

ls_columna	=	dwo.Name

CHOOSE CASE ls_columna
	CASE "espe_codigo"
//		IF Duplicado(ls_columna,data) THEN
//			This.SetItem(row, ls_columna, Integer(ias_campo[1]))
//			RETURN 1
//		END IF
		idwc_variedad.Retrieve(Integer(data))
		idwc_calibre.Retrieve(Integer(data), 1)
		
	CASE "vari_codigo"
//		IF Duplicado(ls_columna,data) THEN
//			This.SetItem(row, ls_columna, Integer(ias_campo[2]))
//			RETURN 1
//		END IF
//		IF idwc_variedad.GetRow() > 0 THEN
//			This.Object.vari_nombre[row]	=	idwc_variedad.GetItemString(idwc_variedad.GetRow(),"vari_nombre")
//		END IF

		li_especie	=	This.Object.espe_codigo[row]
		
		idwc_calibre.Retrieve(li_especie, Integer(data))
		
//	CASE "vaca_calibr"
//
//		IF Duplicado(ls_columna,data) THEN
//			This.SetItem(row, ls_columna, ias_campo[3])
//			RETURN 1
//		END IF

	CASE "emba_codigo"
		IF ExisteEmbalaje(data) = False THEN
			This.SetItem(row, "emba_codigo", ll_null)
			RETURN 1
		END IF	
		
	CASE "fade_estado"
		
		IF Integer(data)=2 OR  Integer(data)=5 THEN
			Messagebox('Cuidado','Códigos de Estado Reservados')
			This.SetItem(row, ls_columna, Integer(ias_campo[9]))
			RETURN 1
		END IF
		
	CASE "fade_cancaj"
		dw_1.Object.fade_cankls[row] = dw_1.Object.enva_pesone[row] * Long(data)
	
END CHOOSE

end event

event dw_1::rowfocuschanged;ib_datos_ok = true
if rowcount() < 1 or getrow() = 0 or ib_borrar then 
	ib_datos_ok = false
else
	//SetRow(il_fila)
	//ScrolltoRow(il_fila)
end if
end event


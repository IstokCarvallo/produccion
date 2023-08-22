$PBExportHeader$w_mant_deta_ctlcalagroproduc.srw
$PBExportComments$Mantenedor de Asociación de Agrónomos / Productores.
forward
global type w_mant_deta_ctlcalagroproduc from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_ctlcalagroproduc from w_mant_detalle_csd
integer x = 123
integer y = 96
integer width = 2711
integer height = 1264
string title = "PRODUCTORES / AGRONOMOS"
end type
global w_mant_deta_ctlcalagroproduc w_mant_deta_ctlcalagroproduc

type variables
DataWindowChild	 	dw_packing, dw_cliente, idwc_agronomo, idwc_agroreem, idwc_productor, &
							idwc_zona, idwc_cliente
uo_ctlcalagronomos	iuo_ctlcalagronomos
uo_productores			iuo_productores

end variables

forward prototypes
public function boolean noexistecliente (integer ai_cliente)
public function boolean noexisteproductor (long al_productor)
protected function boolean duplicado (string as_valor, integer ai_tipo)
end prototypes

public function boolean noexistecliente (integer ai_cliente);String	ls_nombre

SELECT	clie_nombre
	INTO	:ls_nombre
	FROM	dba.clientesprod
	WHERE	clie_codigo	=	:ai_Cliente ;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla clientesprod")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente (" + String(ai_Cliente, '000') + &
	 			 "), no ha sido~r" + "ingresado en tabla respectiva.~r~r" + &
				  "Ingrese o seleccione otro Código.")

	RETURN True
ELSE
//	dw_1.SetItem(il_Fila, "variedades_vari_nombre", ls_Nombre)
	RETURN False
END IF

end function

public function boolean noexisteproductor (long al_productor);Integer  li_Cliente, li_Zona
String	ls_Productor

li_Cliente	=	Integer(dw_1.Object.clie_codigo[il_Fila])
li_Zona		=	Integer(dw_1.Object.zona_codigo[il_Fila])

IF IsNull(al_Productor) = False THEN
	SELECT	prod_nombre
		INTO	:ls_Productor
		FROM	dba.productores
		WHERE	zona_codigo	= :li_Zona
		AND	prod_codigo	= :al_Productor ;

	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Productores")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Código de Productor (" + String(al_productor, '00000') + &
		 			 "), no ha sido~r" + "ingresado en tabla respectiva.~r~r" + &
					  "Ingrese o seleccione otro Código.")

		RETURN True
	ELSE
		dw_1.SetItem(il_fila, "prod_nombre", ls_Productor)
	END IF
END IF

RETURN False
end function

protected function boolean duplicado (string as_valor, integer ai_tipo);Long     ll_fila,ll_productor
Integer	li_agronomo, li_cliente


li_agronomo		=	dw_1.Object.ccag_codigo[il_fila]
ll_productor	=	dw_1.Object.prod_codigo[il_fila]

CHOOSE CASE ai_tipo

	CASE 1
		li_agronomo		=	Integer(as_Valor)

	CASE 3
		ll_productor	=	Long(as_valor)

END CHOOSE

ll_fila = dw_1.Find("ccag_codigo = " + String(li_agronomo) + &
							" AND prod_codigo = " + String(ll_productor), + &
							1, dw_1.RowCount())

IF ll_fila > 0 AND ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

on w_mant_deta_ctlcalagroproduc.create
call super::create
end on

on w_mant_deta_ctlcalagroproduc.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = String(dw_1.Object.ccag_codigo[il_Fila])
ias_campo[3] = String(dw_1.Object.prod_codigo[il_Fila])
ias_campo[4] = String(dw_1.Object.ccap_agrrem[il_Fila])

dw_1.SetItem(il_fila, "zona_codigo", Integer(istr_mant.argumento[1]))

IF istr_mant.agrega = False AND istr_mant.borra = False THEN
	dw_1.Object.ccag_codigo.Protect	=	1
	dw_1.Object.ccag_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_1.Object.prod_codigo.Protect	=	1
	dw_1.Object.prod_codigo.BackGround.Color	=	RGB(192,192,192)
END IF
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "ccag_codigo", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "prod_codigo", Long(ias_campo[3]))
	dw_1.SetItem(il_fila, "ccap_agrrem", Integer(ias_campo[4]))
END IF
end event

event ue_antesguardar();String	ls_mensaje, ls_colu[]
Integer	li_cont

IF Isnull(dw_1.Object.ccag_codigo[il_fila]) OR dw_1.Object.ccag_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Agrónomo"
	ls_colu[li_cont]	= "ccag_codigo"
END IF

IF Isnull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Productor"
	ls_colu[li_cont]	= "prod_codigo"
END IF

IF Isnull(dw_1.Object.ccap_agrrem[il_fila]) OR dw_1.Object.ccap_agrrem[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Agrónomo Reemplazado"
	ls_colu[li_cont]	= "ccap_agrrem"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetItem(il_fila, "zona_codigo", Integer(istr_mant.argumento[1]))


end event

event open;x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")
istr_mant = Message.PowerObjectParm
istr_mant.dw.ShareData(dw_1)

dw_1.GetChild("zona_codigo", idwc_zona)
idwc_zona.SetTransObject(SQLCA)
idwc_zona.Retrieve()

dw_1.GetChild("ccag_codigo", idwc_agronomo)
idwc_agronomo.SetTransObject(SQLCA)
idwc_agronomo.Retrieve(Integer(istr_mant.argumento[1]))

dw_1.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(SQLCA)
idwc_productor.Retrieve(integer(istr_mant.argumento[1]))

dw_1.GetChild("ccap_agrrem", idwc_agroreem)
idwc_agroreem.SetTransObject(SQLCA)
idwc_agroreem.Retrieve(Integer(istr_mant.argumento[1]))

dw_1.SetTransObject(sqlca)




end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_ctlcalagroproduc
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_ctlcalagroproduc
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_ctlcalagroproduc
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_ctlcalagroproduc
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_ctlcalagroproduc
integer x = 2405
integer y = 384
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_ctlcalagroproduc
integer x = 2400
integer y = 168
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_ctlcalagroproduc
integer x = 2400
integer y = 600
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_ctlcalagroproduc
integer y = 112
integer width = 2158
integer height = 904
string dataobject = "dw_mant_ctlcalagroproduc"
end type

event dw_1::itemchanged;call super::itemchanged;Long		ll_fila
String	ls_campo, ls_Null

SetNull(ls_Null)

ls_campo = GetColumnName()

iuo_ctlcalagronomos	=	Create uo_ctlcalagronomos
iuo_productores      =  Create uo_productores

CHOOSE CASE ls_campo

	CASE "ccag_codigo"
		IF iuo_ctlcalagronomos.ofp_recupera_ctlcalagronomos(sqlca,Integer(istr_mant.argumento[1]), &
			Integer(data),True) OR Duplicado(data, 1) THEN
			This.SetItem(il_fila,"ccag_nombre", iuo_ctlcalagronomos.is_ccag_nombre)
		ELSE
			This.SetItem(il_fila, "ccag_codigo", Integer(ls_Null))
			This.SetItem(il_fila, "ccag_nombre", ls_Null)
			RETURN 1
		END IF

	
	CASE "prod_codigo"
		IF Not iuo_productores.existe(Long(data),True,sqlca) OR Duplicado(data, 3) THEN
			This.SetItem(il_fila, "prod_codigo", Long (ls_Null))
			RETURN 1
		END IF

	CASE "ccap_agrrem"
		IF iuo_ctlcalagronomos.ofp_recupera_ctlcalagronomos(sqlca,Integer(istr_mant.argumento[1]), &
			Integer(data),True) THEN
			This.SetItem(il_fila,"ccap_agrrem", iuo_ctlcalagronomos.is_ccag_nombre)
		ELSE
			
			This.SetItem(il_fila, "ccap_agrrem", Integer(ls_Null))
			This.SetItem(il_fila, "ccag_nombre", ls_Null)
			RETURN 1
		END IF

END CHOOSE
end event


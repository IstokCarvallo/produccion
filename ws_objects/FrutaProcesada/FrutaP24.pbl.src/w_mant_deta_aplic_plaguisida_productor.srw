$PBExportHeader$w_mant_deta_aplic_plaguisida_productor.srw
forward
global type w_mant_deta_aplic_plaguisida_productor from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_aplic_plaguisida_productor from w_mant_detalle_csd
integer width = 3218
integer height = 1372
end type
global w_mant_deta_aplic_plaguisida_productor w_mant_deta_aplic_plaguisida_productor

type variables
Integer ii_estado

DataWindowChild	idwc_productor, idwc_predio
end variables

forward prototypes
public function boolean noexisteproductor (long al_productor)
public function boolean duplicado (long al_productor)
end prototypes

public function boolean noexisteproductor (long al_productor);String	ls_nombre, ls_embarque, ls_patente
Integer	li_cliente, li_especie, li_planta
Long		ll_productor
	
SELECT	prod_nombre
	INTO	:ls_nombre
	FROM	dbo.productores
	WHERE prod_codigo = :al_productor;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Produtores")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Productor no ha sido creado, Ingrese otro Código.", &
			Exclamation!, OK!)
	RETURN True
ELSE
		
	li_cliente 	= dw_1.Object.clie_codigo[1]
	li_planta 	= dw_1.Object.plde_codigo[1]
	ls_embarque = dw_1.Object.embq_codigo[1]
	li_especie 	= dw_1.Object.espe_codigo[1]
	ls_patente 	= dw_1.object.decl_patent[1]
	
	SELECT Distinct prod_codigo
	INTO	:ll_productor
	FROM dbo.despafrigoen as enc, dbo.despafrigode as det, dbo.palletfruta as pan
	WHERE enc.clie_codigo =	:li_cliente
	AND	enc.embq_codigo =	:ls_embarque
	AND	enc.plde_codigo = :li_planta
	AND   enc.defe_patent = :ls_patente
	AND	enc.clie_codigo = det.clie_codigo
	AND	enc.plde_codigo = det.plde_codigo
	AND	enc.defe_numero = det.defe_numero
	AND	enc.clie_codigo = pan.clie_codigo
	AND	enc.plde_codigo = pan.plde_codigo
	AND	det.paen_numero = pan.paen_numero
	AND	pan.prod_codigo = :al_productor
	AND	pan.espe_codigo = :li_especie;
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Produtores")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Productor NO Existe en ese Camion, Ingrese otro Código.", &
				Exclamation!, OK!)
		RETURN True
	ELSE
		RETURN False
	END IF	
END IF
end function

public function boolean duplicado (long al_productor);Long		ll_fila
Integer	li_cliente, li_planta, li_cantid

ll_fila	=	dw_1.Find("clie_codigo = " + istr_mant.Argumento[3] + &
						" AND plde_codigo = " + istr_mant.Argumento[1] + &
						" AND espe_codigo = " + istr_mant.Argumento[2] + &
						" AND embq_codigo = '" + istr_mant.Argumento[4] +"'"+ &
						" AND decl_patent = '" + istr_mant.Argumento[5]+"'" + &
						" AND prod_codigo = " + String(al_productor) , 1, istr_mant.dw.RowCount())
	
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Productor ya fue incluido en Detalle", Information!, Ok!)
	RETURN True
END IF

RETURN False
end function

on w_mant_deta_aplic_plaguisida_productor.create
call super::create
end on

on w_mant_deta_aplic_plaguisida_productor.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1]	=	String(dw_1.Object.clie_codigo[il_fila])
ias_campo[2]	=	String(dw_1.Object.plde_codigo[il_fila])
ias_campo[3]	=	String(dw_1.Object.espe_codigo[il_fila])
ias_campo[4]	=	String(dw_1.Object.prod_codigo[il_fila])
ias_campo[5]	=	String(dw_1.Object.decl_fecdec[il_fila], 'dd/mm/yyyy')
ias_campo[6]	=	String(dw_1.Object.decl_predio[il_fila])
ias_campo[7]	=	String(dw_1.Object.decl_secuen[il_fila])

IF istr_mant.agrega THEN
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))
	dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "prod_codigo", Long(istr_mant.argumento[4]))
	dw_1.SetItem(il_fila, "decl_fecdec", Date(istr_mant.argumento[5]))
	dw_1.SetItem(il_fila, "decl_predio", Integer(istr_mant.argumento[6]))
	dw_1.SetItem(il_fila, "decl_secuen", il_fila)
END IF


end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]
Date	ld_fecha

IF Isnull(dw_1.Object.decl_procom[il_fila]) OR dw_1.Object.decl_procom[il_fila] = '' THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~Producto Comercial"
	ls_colu[li_cont]	= "decl_procom"
END IF

IF Isnull(dw_1.Object.decl_ingact[il_fila]) OR dw_1.Object.decl_ingact[il_fila] = '' THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nIngrediente Activo"
	ls_colu[li_cont]	= "decl_ingact"
END IF

IF Isnull(dw_1.Object.decl_fecapl[il_fila]) OR dw_1.Object.decl_fecapl[il_fila] = ld_fecha THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nFecha Aplicación"
	ls_colu[li_cont]	= "decl_fecapl"
END IF

IF Isnull(dw_1.Object.decl_apldos[il_fila]) OR dw_1.Object.decl_apldos[il_fila] = '' THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nDosis Aplicada"
	ls_colu[li_cont]	= "decl_apldos"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus() 
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))
dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "prod_codigo", Long(istr_mant.argumento[4]))
dw_1.SetItem(il_fila, "decl_fecdec", Date(istr_mant.argumento[5]))
dw_1.SetItem(il_fila, "decl_predio", Integer(istr_mant.argumento[6]))
dw_1.SetItem(il_fila, "decl_secuen", il_fila)

dw_1.SetColumn("decl_procom")
dw_1.SetFocus()

end event

event open;call super::open;dw_1.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve(-1)

dw_1.GetChild("decl_predio", idwc_predio)
idwc_predio.SetTransObject(sqlca)
idwc_predio.Retrieve(Long(istr_mant.argumento[4]))




end event

event ue_deshace;call super::ue_deshace;dw_1.Object.clie_codigo[il_fila]		= Integer(ias_campo[1])
dw_1.Object.plde_codigo[il_fila] 	= Integer(ias_campo[2])
dw_1.Object.espe_codigo[il_fila] 	= Integer(ias_campo[3])
dw_1.Object.prod_codigo[il_fila] 	= Integer(ias_campo[4])
dw_1.Object.decl_fecdec[il_fila] 	= Date(ias_campo[5])
dw_1.Object.decl_predio[il_fila] 	= Integer(ias_campo[6])
dw_1.Object.decl_secuen[il_fila] 	= Integer(ias_campo[7])
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_aplic_plaguisida_productor
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_aplic_plaguisida_productor
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_aplic_plaguisida_productor
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_aplic_plaguisida_productor
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_aplic_plaguisida_productor
integer x = 2853
integer y = 688
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_aplic_plaguisida_productor
integer x = 2853
integer y = 472
end type

event pb_acepta::clicked;istr_mant.respuesta = 1

IF istr_mant.agrega THEN
	Parent.TriggerEvent("ue_nuevo")
ELSE
	CloseWithReturn(Parent, istr_mant)
END IF

end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_aplic_plaguisida_productor
integer x = 2853
integer y = 904
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_aplic_plaguisida_productor
integer x = 91
integer width = 2633
integer height = 1088
string dataobject = "dw_mant_aplic_deta_plaguisida_productor"
end type

event dw_1::itemchanged;Integer	li_null
String	ls_columna

SetNull(li_null)
ls_columna = GetColumnName()

CHOOSE CASE ls_columna
	CASE "prod_codigo"
		IF Duplicado(Long(data)) OR NoExisteproductor(Long(data)) THEN
			This.SetItem(il_fila, ls_columna, li_null)
			RETURN 1
		END IF
		
END CHOOSE
end event


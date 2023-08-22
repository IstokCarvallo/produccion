$PBExportHeader$w_mant_deta_aplic_plaguisida.srw
forward
global type w_mant_deta_aplic_plaguisida from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_aplic_plaguisida from w_mant_detalle_csd
integer width = 3282
integer height = 1148
end type
global w_mant_deta_aplic_plaguisida w_mant_deta_aplic_plaguisida

type variables
Integer ii_estado

DataWindowChild	idwc_productor
end variables

forward prototypes
public function boolean noexisteproductor (long al_productor)
public function boolean duplicado (long al_productor)
end prototypes

public function boolean noexisteproductor (long al_productor);String	ls_nombre, ls_embarque, ls_patente
Integer	li_cliente, li_especie, li_planta
Long		ll_productor

li_cliente 	= dw_1.Object.clie_codigo[1]
	
SELECT	pro.prod_nombre
	INTO	:ls_nombre
	FROM	dbo.productores as pro,dbo.productoresclientes as cli
	WHERE pro.prod_codigo = :al_productor
	AND	pro.prod_codigo = cli.prod_codigo
	AND	cli.clie_codigo = :li_cliente;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Produtores")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de productor no ha sido creado o pertenece a otro cliente, Ingrese otro Código.", &
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
						" AND prod_codigo = " + String(al_productor) , 1, dw_1.RowCount())
	
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Productor ya fue incluido en Detalle", Information!, Ok!)
	RETURN True
END IF

RETURN False
end function

on w_mant_deta_aplic_plaguisida.create
call super::create
end on

on w_mant_deta_aplic_plaguisida.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;dw_1.modify("buscapallet.Visible=1")

IF istr_mant.agrega THEN
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))
	dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "embq_codigo", (istr_mant.argumento[4]))
	dw_1.SetItem(il_fila, "decl_patent", (istr_mant.argumento[5]))
END IF


end event

event ue_deshace;call super::ue_deshace;//dw_1.Object.paen_numero[il_fila]	=	Long(ias_campo[1])
//dw_1.Object.defe_termog[il_fila]	=	Long(ias_campo[2])
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nProductor"
	ls_colu[li_cont]	= "prod_codigo"
END IF

IF Isnull(dw_1.Object.decl_totprd[il_fila]) OR dw_1.Object.decl_totprd[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCajas Productor"
	ls_colu[li_cont]	= "decl_totprd"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus() 
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;ib_ok = True

This.TriggerEvent("ue_guardar")
IF Message.DoubleParm = -1 THEN ib_ok = False

IF ib_ok = False THEN RETURN

//	pb_salir.TriggerEvent(Clicked!)

IF dw_1.Object.totalcajas[1] >= Integer(istr_mant.Argumento[6]) THEN
	MessageBox("Atención", "Se ha completado la cantidad de Cajas.")
	
	pb_salir.TriggerEvent(Clicked!)
ELSE
	wf_nuevo()
	
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))
	dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "embq_codigo", Long(istr_mant.argumento[4]))
	dw_1.SetItem(il_fila, "decl_patent", Date(istr_mant.argumento[5]))

	dw_1.SetColumn("prod_codigo")
	dw_1.SetFocus()
END IF
end event

event open;call super::open;dw_1.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve(Integer(istr_mant.argumento[3]))




end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_aplic_plaguisida
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_aplic_plaguisida
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_aplic_plaguisida
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_aplic_plaguisida
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_aplic_plaguisida
integer x = 2834
integer y = 428
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_aplic_plaguisida
integer x = 2834
integer y = 212
end type

event pb_acepta::clicked;istr_mant.respuesta = 1

IF istr_mant.agrega THEN
	IF dw_1.object.totalcajas[1] < Integer(istr_mant.argumento[6]) THEN
		Parent.TriggerEvent("ue_nuevo")
	ELSE
		
		MessageBox("ADVERTENCIA","Se ha completado la cantidad de CAJAS~r" + &
						"~r~rSe Retornará a la Ventana de Mantención.", &
						Information!, OK!)
			
	  	parent.TriggerEvent("ue_antesguardar")
		
		IF Message.DoubleParm = -1 THEN RETURN
	  
						
	  CloseWithReturn(Parent, istr_mant)
	END IF
ELSE
	MessageBox("ADVERTENCIA","Se ha completado la cantidad de Pallets~r" + &
						"~r~rSe Retornará a la Ventana de Mantención.", &
						Information!, OK!)
	CloseWithReturn(Parent, istr_mant)
END IF

end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_aplic_plaguisida
integer x = 2834
integer y = 644
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_aplic_plaguisida
integer x = 73
integer y = 116
integer width = 2629
integer height = 904
string dataobject = "dw_mant_aplic_deta_plaguisida"
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


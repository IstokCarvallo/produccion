$PBExportHeader$w_mant_deta_despafrigode.srw
forward
global type w_mant_deta_despafrigode from w_mant_detalle_csd
end type
type cb_1 from commandbutton within w_mant_deta_despafrigode
end type
end forward

global type w_mant_deta_despafrigode from w_mant_detalle_csd
integer width = 3278
integer height = 1320
cb_1 cb_1
end type
global w_mant_deta_despafrigode w_mant_deta_despafrigode

type variables
Integer ii_estado
end variables

forward prototypes
public function boolean duplicado (long al_numero)
public function boolean noexistepallet (long al_numero)
public subroutine buscapallet ()
end prototypes

public function boolean duplicado (long al_numero);Long		ll_fila
Integer	li_cliente, li_planta, li_cantid

ll_fila	=	dw_1.Find("clie_codigo = " + istr_mant.Argumento[3] + &
						" AND plde_codigo = " + istr_mant.Argumento[1] + &
						" AND paen_numero = " + String(al_Numero) , 1, dw_1.RowCount())
	
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Pallet ya fue incluido en Detalle de Embarque", Information!, Ok!)
	RETURN True
END IF

RETURN False
end function

public function boolean noexistepallet (long al_numero);String	ls_nomvar, ls_embala
Integer	li_catego, li_cliente, li_ContCalidad, li_tipopa, li_cajas, li_inspec, &
			li_Destino, li_Especie, li_Variedad, li_planta, li_fumiga, li_vencimiento,& 
			li_cont, li_estado, li_tipopallet, li_etiqueta
Date		ld_fechainpe, ld_fechafumi
Long		ll_numfumi

li_cliente	= 	Integer(istr_mant.argumento[3])
li_planta	=	Integer(istr_mant.argumento[1])

SELECT	Distinct pae.paen_tipopa, var.vari_nombre, pae.emba_codigo, pae.cate_codigo,
			pae.paen_concal, pae.paen_ccajas, pae.paen_inspec, pae.dest_codigo,
			pae.espe_codigo, pae.vari_codigo, pae.paen_estado, pae.cond_codigo,
			isnull(var.vari_vencim,0),fumi_fecfum,fumi_numero,pae.copa_codigo,pae.etiq_codigo
	INTO	:li_tipopa, :ls_nomvar, :ls_embala, :li_catego, :li_ContCalidad, :li_cajas,
			:li_inspec, :li_Destino, :li_Especie, :li_Variedad, :ii_estado, :li_fumiga,
			:li_vencimiento, :ld_fechafumi, :ll_numfumi, :li_tipopallet, :li_etiqueta
	FROM	dbo.palletencab as pae, dbo.variedades as var,dbo.palletfruta as pal
	WHERE pae.clie_codigo	= 	:li_cliente
	AND	pae.paen_numero	= 	:al_numero
	AND	pae.plde_codigo	=	:li_planta
	AND	var.espe_codigo	= 	pae.espe_codigo
	AND	var.vari_codigo	= 	pae.vari_codigo 
	AND 	pae.clie_codigo	=	pal.clie_codigo
	AND 	pae.plde_codigo	=	pal.plde_codigo
	AND	pae.paen_numero 	= 	pal.paen_numero;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Palletencab")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Pallet no ha sido creado, Ingrese otro Código.", &
			Exclamation!, OK!)
	RETURN True
END IF

SELECT	count(*)
	INTO	:li_cont
	FROM	dbo.palletfruta as pae,dbo.plantadesp as pld
	WHERE pae.clie_codigo	= 	:li_cliente
	AND	pae.paen_numero	= 	:al_numero
	AND	pae.plde_codigo	=	:li_planta
	AND	pae.pafr_copack	=	pld.plde_codigo
	AND	pld.plde_blkwal	=	1;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla palletfruta")
	RETURN True	
END IF
//IF NOT isnull(istr_mant.argumento[11]) OR istr_mant.argumento[11] <> '' THEN
	
IF (li_Especie <> Integer(istr_mant.argumento[10])) OR isnull(istr_mant.argumento[10]) THEN
		
//	IF isnull(istr_mant.argumento[12]) THEN
		MessageBox("Atención", "Especie Pallet No corresponde a Especie Encabezado.", &
		Exclamation!, OK!)
		RETURN True
//	END IF	
		
END IF
	
//END IF	

IF ii_estado = 2 THEN
	MessageBox("Atención", "Pallet ya fue Despachado desde Planta Despachadora.", &
			Exclamation!, OK!)
	RETURN True
ELSEIF ii_estado = 3 THEN
	MessageBox("Atención", "Pallet fue Repalletizado en Planta Despachadora.", &
			Exclamation!, OK!)
	RETURN True
END IF

IF li_ContCalidad<>1 THEN
	MessageBox("Atención", "Pallet Objetado o Rechazado por C.Calidad.~r~r" + &
			"No Cumple Condición para Despacho", Exclamation!, OK!)
	RETURN True
END IF

/*	Avisa si se embarca un Pucho	*/
IF li_tipopa = 2 THEN
	IF MessageBox("Advertencia", "Se está Despachando un Pucho.~r~r" + &
			"Desea continuar ?", Question!, YesNo!, 2) = 2 THEN RETURN True
END IF

/*	Packing Bloqueado	*/
IF li_cont > 0 THEN
	IF MessageBox("Advertencia", "Packing Con Bloqueo WalMart.~r~r" + &
			"Desea continuar ?", Question!, YesNo!, 2) = 2 THEN RETURN True
END IF

/*	Avisa si se embarca un Pallet Rechazado por SAG	*/
IF li_inspec = 3 THEN
	IF MessageBox("Advertencia", "Se está Despachando un Pallet Rechazado por el SAG.~r~r" + &
			"Desea continuar ?", Question!, YesNo!, 2) = 2 THEN RETURN True
END IF

/*	Avisa si se embarca un Pallet pendiente de inspeccion	*/
IF istr_mant.argumento[20] = '30' THEN
	IF li_inspec = 5 THEN
		MessageBox("Advertencia", "Esta Despachando un Pallet con Inspección Pendiente.~r~r", + &
		Exclamation!, OK!)  
	END IF
ELSE
	IF li_inspec = 5 THEN
		MessageBox("Advertencia", "NO Puede Despachar un Pallet con Inspección Pendiente.~r~r", + &
		Exclamation!, OK!)  
		RETURN True
	END IF
END IF	

SELECT fumi_estado
INTO :li_estado
FROM dbo.fumigaenc
WHERE  clie_codigo	= 	:li_cliente
	AND	plde_codigo	=	:li_planta
	AND	fumi_numero	=	:ll_numfumi
	AND	fumi_fecfum =  :ld_fechafumi;

/*	Verifica Condición de Inspección	*/
IF istr_mant.argumento[20] = '30' THEN
	IF li_estado = 7 THEN
		MessageBox("Advertencia", "Esta Despachando un Pallet con Condición Pendiente.~r~r", + &
		Exclamation!, OK!)  
	END IF
ELSE
	IF li_estado = 7 THEN
		MessageBox("ATENCION", "SE ESTA DESPACHANDO UN FOLIO CON CONDICION PENDIENTE.", Question!, Ok!) 
		RETURN True
	END IF	
END IF	

IF li_estado = 2 THEN
	IF MessageBox("Advertencia", "Se está Despachando un Pallet con Proceso Condicón Rechazado.~r~r" + &
			"Desea continuar ?", Question!, YesNo!, 2) = 2 THEN RETURN True
END IF

IF (IsNull(li_inspec) OR li_inspec = 0) AND &
	(Integer(istr_mant.argumento[27])=7 OR Integer(istr_mant.argumento[27])=8) THEN //AND &
	//(Integer(istr_mant.argumento[7]) <> 225) THEN
		IF MessageBox("ATENCION", "SE ESTA DESPACHANDO UN FOLIO NO INSPECCIONADO." + &
				 "Desea continuar ?", Question!, YesNo!, 1) = 2 THEN RETURN True
	
END IF

IF (IsNull(li_fumiga) OR li_fumiga = 0) AND &
	(Integer(istr_mant.argumento[27])=7 OR Integer(istr_mant.argumento[27])=8) THEN //AND &
	//(Integer(istr_mant.argumento[7]) <> 225) THEN
		IF MessageBox("ATENCION", "SE ESTA DESPACHANDO UN FOLIO SIN CONDICION." + &
				 "Desea continuar ?", Question!, YesNo!, 1) = 2 THEN RETURN True
	
END IF

/*	Verifica Si corresponde Destino de embarque	*/
IF li_Destino <> 999 THEN
 	IF Integer(istr_mant.argumento[7]) > 0 AND &
		Integer(istr_mant.argumento[7]) <> li_Destino THEN
		IF MessageBox("Advertencia", "Destino de Embarque no Corresponde al Inspeccionado." + &
					"Desea continuar ?", Question!, YesNo!, 1) = 2 THEN RETURN True
	END IF
END IF
 
IF li_inspec = 1 THEN
	SELECT max(inpd_fechai)
	INTO :ld_fechainpe
	FROM dbo.inspecpaldet
	WHERE inpe_tipoin in (1,2)
	AND	clie_codigo = :li_cliente
	AND	plde_codigo = :li_planta
	AND	paen_numero = :al_numero
	AND	isnull(inpd_nroanu,0) = 0
	AND	isnull(inpd_frecha,'1900-01-01') = '1900-01-01';
	
	IF li_vencimiento = 0 THEN
		MessageBox("Atención", "Falta Mantención en Tabla Variedades (Vencimiento).", &
				Exclamation!, OK!)
		RETURN True				
	ELSE				
		IF RelativeDate(ld_fechainpe,li_vencimiento) < Today() THEN
			MessageBox("Atención", "Se Encuentra Plazo Vencido de Inspección.", &
				Exclamation!, OK!)
			RETURN True
		END IF	
	END IF
END IF

dw_1.SetItem(il_fila, "paen_numero", al_numero)
dw_1.SetItem(il_fila, "espe_codigo", li_Especie)
dw_1.SetItem(il_fila, "vari_codigo", li_Variedad)
dw_1.SetItem(il_fila, "variedades_vari_nombre", ls_nomvar)
dw_1.SetItem(il_fila, "palletencab_paen_tipopa", li_tipopa)
dw_1.SetItem(il_fila, "palletencab_emba_codigo", ls_embala)
dw_1.SetItem(il_fila, "palletencab_paen_concal", li_ContCalidad)
dw_1.SetItem(il_fila, "paen_ccajas", li_cajas)
dw_1.SetItem(il_fila, "palletencab_paen_inspec", li_inspec)
dw_1.SetItem(il_fila, "palletencab_dest_codigo", li_Destino)
dw_1.SetItem(il_fila, "copa_codigo", li_tipopallet)
dw_1.SetItem(il_fila, "etiq_codigo", li_etiqueta)

RETURN False
end function

public subroutine buscapallet ();dw_1.Modify("buscapallet.border = 0")
dw_1.Modify("buscapallet.border = 5")

istr_busq.argum[4]	=	""
istr_busq.argum[5]	=	""
istr_busq.argum[6]	=	""
istr_busq.argum[7]	=	""
istr_busq.argum[8]	=	""
istr_busq.argum[9]	=	""
istr_busq.argum[10]	=	""
istr_busq.argum[11]	=	""
istr_busq.argum[2]	=	""
istr_busq.argum[1]	=	istr_mant.argumento[3]  //String(dw_1.Object.clie_codigo[il_fila])
istr_busq.argum[12]	=	istr_mant.argumento[1]	//String(dw_1.Object.plde_codigo[il_fila])
istr_busq.argum[3]	=	"1"

OpenWithParm(w_busc_palletencab, istr_busq)

istr_busq	       	=	Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	
	NoExistePallet(Long(istr_busq.Argum[2]))
	
	IF Duplicado(Long(istr_busq.Argum[2])) THEN
		dw_1.SetFocus()
	ELSE
		pb_acepta.SetFocus()
	END IF
ELSE
	dw_1.SetColumn("paen_numero")
	dw_1.SetFocus()
END IF

dw_1.Modify("buscapallet.border = 0")
dw_1.Modify("buscapallet.border = 6")

RETURN
end subroutine

on w_mant_deta_despafrigode.create
int iCurrent
call super::create
this.cb_1=create cb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_1
end on

on w_mant_deta_despafrigode.destroy
call super::destroy
destroy(this.cb_1)
end on

event ue_recuperadatos;call super::ue_recuperadatos;dw_1.Object.BuscaPallet.Visible	=	1

ias_campo[1]	=	String(dw_1.Object.paen_numero[il_fila])
ias_campo[2]	=	String(dw_1.Object.defe_termog[il_fila])

IF istr_mant.agrega THEN
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))
	dw_1.SetItem(il_fila, "defe_numero", Long(istr_mant.argumento[2]))
END IF

IF Not istr_mant.Agrega And Not istr_mant.Borra THEN
		dw_1.Object.paen_numero.Protect				=	1
		dw_1.Object.paen_numero.Color 					= RGB(255,255,255)
		dw_1.Object.paen_numero.BackGround.Color	=	553648127
		
		dw_1.Object.BuscaPallet.Visible	=	0
END IF

end event

event ue_deshace;call super::ue_deshace;dw_1.Object.paen_numero[il_fila]	=	Long(ias_campo[1])
dw_1.Object.defe_termog[il_fila]	=	String(ias_campo[2])
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.paen_numero[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNúmero de Pallet"
	ls_colu[li_cont]	= "paen_numero"
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

IF dw_1.RowCount() >= Integer(istr_mant.Argumento[4]) THEN
	MessageBox("Atención", "No puede ingresar más Pallets.")
	
	pb_salir.TriggerEvent(Clicked!)
ELSEIF dw_1.Object.totcajas[1] >= Integer(istr_mant.Argumento[8]) THEN
	MessageBox("Atención", "Se ha completado la cantidad de Cajas, No podrá ingresar más Pallets.")
	
	pb_salir.TriggerEvent(Clicked!)
ELSE
	wf_nuevo()
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "defe_numero", Long(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))

	dw_1.SetColumn("paen_numero")
	dw_1.SetFocus()
END IF
end event

event resize;call super::resize;cb_1.x	=	pb_salir.x
cb_1.y	=	pb_salir.y + 255
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_despafrigode
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_despafrigode
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_despafrigode
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_despafrigode
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_despafrigode
integer x = 2880
integer y = 388
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_despafrigode
integer x = 2880
integer y = 172
end type

event pb_acepta::clicked;IF istr_mant.argumento[27] = '7' OR istr_mant.argumento[27] = '8' OR istr_mant.argumento[27] = '9' OR istr_mant.argumento[27] = '11' THEN
	IF isnull(dw_1.Object.defe_ladoes[il_fila]) OR &
		isnull(dw_1.Object.defe_filaes[il_fila]) OR dw_1.Object.defe_filaes[il_fila] = 0 THEN
		MessageBox("Atención", "Falta Ingresar Estiba de Pallet.", &
				Exclamation!, OK!)
		RETURN 1
	END IF	
END IF	

istr_mant.respuesta = 1

IF istr_mant.agrega THEN
	IF (dw_1.Rowcount()<Integer(istr_mant.argumento[4])) THEN
		IF Long(dw_1.object.totcajas[1]) < Long(istr_mant.argumento[8]) THEN
			Parent.TriggerEvent("ue_nuevo")
		ELSE
			MessageBox("               ADVERTENCIA","Se ha completado la cantidad de CAJAS~r" + &
							"indicados en el Despacho.~r~rSe Retornará a la Ventana de Mantención.", &
							Information!, OK!)
		  CloseWithReturn(Parent, istr_mant)
		END IF
	ELSE
		MessageBox("               ADVERTENCIA","Se ha completado la cantidad de Pallets~r" + &
							"indicados en el Despacho.~r~rSe Retornará a la Ventana de Mantención.", &
							Information!, OK!)
		CloseWithReturn(Parent, istr_mant)
	END IF
ELSE
	CloseWithReturn(Parent, istr_mant)
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_despafrigode
integer x = 2885
integer y = 604
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_despafrigode
integer x = 101
integer y = 108
integer width = 2647
integer height = 1044
string dataobject = "dw_mant_despafrigode"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_Nula
Integer	li_cliente,pallet,li_status,li_tipo

SetNull(ls_Nula)

ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "paen_numero"
		IF len(data) = 18 THEN
			data = String(mid(data,12,6))
			dw_1.Object.paen_numero[il_fila] = Long(data)
		END IF
		
		IF NoExistePallet(Long(data)) OR Duplicado(Long(data)) THEN
			dw_1.SetItem(il_fila, ls_columna, Long(ls_Nula))
			RETURN 1
		END IF

END CHOOSE
end event

event dw_1::clicked;call super::clicked;String	ls_columna

ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "buscapallet"
		BuscaPallet()
		
END CHOOSE
end event

type cb_1 from commandbutton within w_mant_deta_despafrigode
integer x = 2885
integer y = 888
integer width = 302
integer height = 108
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Detalle"
end type

event clicked;/*
	Argumentos de istr_mant2 => Recepción por Ingreso o Consulta
		Argumento	[1]	=	Código de Planta
						[2]	=	Número de Folio Recepción
						[3]	=	Código de Exportador
						[4]	=	Cantidad de Tarjas
						[5]	=	Tipo de packing
						[6]	=	Número de Pallet
						[7]	=	Condición de Sólo Consulta (0 = No / 1 = Si)
						[8]	=	
						[9]	=	Código de Especie
						[10]	=	Código de Variedad
						[11]	=	
						[12]	=	
						[13]	=	Nombre de Condición
						[14]	=	Nombre de Planta
						[15]	=	
						[16]	=	Condición de Sólo Consulta (0 = No / 1 = Si)
						[20]	=	Tipo de Recepción : 1 Ingreso desde Packing => Recfruprocee_particular	
						[21]	=	Packing Origen
*/
Str_mant		lstr_mant

IF dw_1.RowCount() > 0 THEN
		lstr_mant.Agrega			=	False
		lstr_mant.Borra			=	False
		lstr_mant.dw				=	dw_1
		lstr_mant.Argumento[3]	=	istr_mant.Argumento[3]
		lstr_mant.Argumento[6]	=	String(dw_1.GetitemNumber(il_fila, "paen_numero"))
		lstr_mant.Argumento[7]	=	'1'
		lstr_mant.Argumento[9]	=	String(dw_1.GetitemNumber(il_fila, "espe_codigo"))
		lstr_mant.Argumento[10]	=	String(dw_1.GetitemNumber(il_fila, "vari_codigo"))
		lstr_mant.Argumento[11]	=	'100'
		lstr_mant.Argumento[12]	=	'100'
		lstr_mant.Argumento[21]	=	''
		
	IF lstr_mant.Argumento[6]<>"" THEN
		OpenWithParm(w_maed_palletencab_consulta, lstr_mant)
	END IF
END IF
end event


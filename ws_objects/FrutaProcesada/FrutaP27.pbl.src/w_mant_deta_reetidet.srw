$PBExportHeader$w_mant_deta_reetidet.srw
$PBExportComments$Ventana Mantención Detalle de Reetiquetado.
forward
global type w_mant_deta_reetidet from w_mant_detalle_csd
end type
type cb_1 from commandbutton within w_mant_deta_reetidet
end type
end forward

global type w_mant_deta_reetidet from w_mant_detalle_csd
integer width = 3154
integer height = 1268
cb_1 cb_1
end type
global w_mant_deta_reetidet w_mant_deta_reetidet

type variables
DataWindowChild	idwc_etiqueta, idwc_etiqant

Integer				ii_estado

end variables

forward prototypes
public subroutine buscapallet ()
public function boolean duplicado (long al_numero)
public function boolean noexistepallet (long al_numero)
end prototypes

public subroutine buscapallet ();dw_1.Modify("buscapallet.border = 0")
dw_1.Modify("buscapallet.border = 5")

istr_busq.argum[2]	=	""
istr_busq.argum[1]	=	String(dw_1.Object.clie_codigo[il_fila])
istr_busq.argum[12]	=	String(dw_1.Object.plde_codigo[il_fila])
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

public function boolean duplicado (long al_numero);Long		ll_Fila
Integer	li_Cliente, li_Planta

ll_Fila	=	dw_1.Find("clie_codigo = " + istr_mant.Argumento[3] + &
					  " AND plde_codigo = " + istr_mant.Argumento[1] + &
					  " AND paen_numero = " + String(al_Numero), 1, dw_1.RowCount())

IF ll_Fila > 0 and ll_Fila <> il_Fila THEN
	MessageBox("Error","Pallet ya fue incluido en Detalle de Reetiquetado", Information!, Ok!)
	RETURN True
END IF

RETURN False
end function

public function boolean noexistepallet (long al_numero);String	ls_nomvar, ls_embala
Integer	li_catego, li_cliente, li_ContCalidad, li_tipopa, li_cajas, li_inspec, &
			li_Destino, li_Especie, li_Variedad, li_planta, li_etiqueta, ll_cont

li_cliente	= 	Integer(istr_mant.argumento[3])
li_planta	=	Integer(istr_mant.argumento[1])

SELECT	pae.paen_tipopa, var.vari_nombre, pae.emba_codigo, pae.cate_codigo,
			pae.paen_concal, pae.paen_ccajas, pae.paen_inspec, pae.dest_codigo,
			pae.espe_codigo, pae.vari_codigo, pae.paen_estado, pae.etiq_codigo
	INTO	:li_tipopa, :ls_nomvar, :ls_embala, :li_catego, :li_ContCalidad, :li_cajas,
			:li_inspec, :li_Destino, :li_Especie, :li_Variedad, :ii_estado, :li_etiqueta
	FROM	dbo.palletencab as pae, dbo.variedades as var
	WHERE pae.clie_codigo	= 	:li_cliente
	AND	pae.paen_numero	= 	:al_numero
	AND	pae.plde_codigo	=	:li_planta
	AND	var.espe_codigo	= 	pae.espe_codigo
	AND	var.vari_codigo	= 	pae.vari_codigo ;
	
SELECT count(*)
	INTO :ll_cont
	FROM dbo.palletfruta
	WHERE clie_codigo	= :li_cliente
	AND	paen_numero	= :al_numero
	AND	plde_codigo	= :li_planta;
	
IF ll_cont = 0 THEN
	MessageBox("Atención", "Pallet NO Existe en Definitivo.", + &
	Exclamation!, OK!)
	RETURN True
END IF	

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Palletencab")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Pallet no ha sido creado, Ingrese otro Código.", &
			Exclamation!, OK!)
	RETURN True
END IF

IF ii_estado = 2 THEN
	MessageBox("Atención", "Pallet ya fue Despachado desde Planta Despachadora.", &
			Exclamation!, OK!)
	RETURN True
END IF

IF ii_estado = 3 THEN
	MessageBox("Atención", "Pallet fue Repalletizado en Planta Despachadora.", &
			Exclamation!, OK!)
	RETURN True
END IF

/*	Verifica Condición de Inspección	*/
//IF li_inspec <> 0  THEN
//	IF MessageBox("Advertencia", "Se quiere Fumigar un Pallet Inspeccionado.~r~r" + &
//					"Desea continuar ?", Question!, YesNo!, 2) = 2 THEN RETURN True
//END IF

dw_1.SetItem(il_fila, "paen_numero", al_numero)
dw_1.SetItem(il_fila, "espe_codigo", li_Especie)
dw_1.SetItem(il_fila, "vari_codigo", li_Variedad)
dw_1.SetItem(il_fila, "vari_nombre", ls_nomvar)
dw_1.SetItem(il_fila, "paen_tipopa", li_tipopa)
dw_1.SetItem(il_fila, "emba_codigo", ls_embala)
dw_1.SetItem(il_fila, "cate_codigo", li_catego)
dw_1.SetItem(il_fila, "palletencab_paen_ccajas", li_cajas)
dw_1.SetItem(il_fila, "etiq_numant", li_etiqueta)

RETURN False
end function

on w_mant_deta_reetidet.create
int iCurrent
call super::create
this.cb_1=create cb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_1
end on

on w_mant_deta_reetidet.destroy
call super::destroy
destroy(this.cb_1)
end on

event ue_recuperadatos;call super::ue_recuperadatos;dw_1.modify("buscapallet.Visible=1")

ias_campo[1]	=	String(dw_1.Object.paen_numero[il_fila])

IF istr_mant.agrega THEN
	dw_1.SetItem(il_fila, "reet_numero", Long(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "reet_fereet", Date(istr_mant.argumento[5]))
END IF

IF Not istr_mant.agrega And Not istr_mant.borra THEN
	dw_1.Object.paen_numero.Protect	=	1
	dw_1.Object.paen_numero.Color 		=	RGB(255,255,255)
	dw_1.Object.paen_numero.BackGround.Color = 553648127
	dw_1.modify("buscapallet.Visible=0")
END IF
end event

event ue_deshace;call super::ue_deshace;dw_1.Object.paen_numero[il_fila]	=	Long(ias_campo[1])

end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.paen_numero[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNúmero de Pallet"
	ls_colu[li_cont]	= "paen_numero"
END IF

IF Isnull(dw_1.Object.etiq_codigo[il_fila]) OR dw_1.Object.etiq_codigo[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nEtiqueta del Pallet"
	ls_colu[li_cont]	= "etiq_codigo"
END IF

IF Isnull(dw_1.Object.etiq_numant[il_fila]) OR dw_1.Object.etiq_numant[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNueva Etiqueta del Pallet"
	ls_colu[li_cont]	= "etiq_numant"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus() 
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;ib_ok = True
Long		ll_cajas

This.TriggerEvent("ue_guardar")
IF Message.DoubleParm = -1 THEN ib_ok = False

IF ib_ok = False THEN RETURN

wf_nuevo()

dw_1.SetItem(il_fila, "reet_numero", Long(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))
dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "reet_fereet", Date(istr_mant.argumento[5]))

dw_1.SetColumn("paen_numero")
dw_1.SetFocus()
end event

event open;istr_mant = Message.PowerObjectParm

dw_1.GetChild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
idwc_etiqueta.Retrieve()

dw_1.GetChild("etiq_numant", idwc_etiqant)
idwc_etiqant.SetTransObject(sqlca)
idwc_etiqant.Retrieve()

call super::open
end event

event resize;call super::resize;cb_1.x		=	pb_Salir.x
cb_1.y		=	pb_Salir.y + pb_Salir.Height + 20
cb_1.Width	=	pb_Salir.Width
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_reetidet
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_reetidet
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_reetidet
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_reetidet
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_reetidet
integer x = 2725
integer y = 368
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_reetidet
integer x = 2725
integer y = 152
end type

event pb_acepta::clicked;call super::clicked;//Long	ll_cajas
//
//istr_mant.respuesta = 1
//
//IF istr_mant.agrega THEN
//	IF (dw_1.Rowcount()<Integer(istr_mant.argumento[4])) THEN
//		ll_cajas	=	Long(dw_1.object.totcajas[1])
//		IF ll_cajas < Long(istr_mant.argumento[8]) THEN
//			Parent.TriggerEvent("ue_nuevo")
//		ELSE
//			MessageBox("               ADVERTENCIA","Se ha completado la cantidad de CAJAS~r" + &
//							"indicados en la Fumigación.~r~rSe Retornará a la Ventana de Mantención.", &
//							Information!, OK!)
//		  CloseWithReturn(Parent, istr_mant)
//		END IF
//	ELSE
//		MessageBox("               ADVERTENCIA","Se ha completado la cantidad de Pallets~r" + &
//							"indicados en la Fumigación.~r~rSe Retornará a la Ventana de Mantención.", &
//							Information!, OK!)
//		CloseWithReturn(Parent, istr_mant)
//	END IF
//ELSE
//	CloseWithReturn(Parent, istr_mant)
//END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_reetidet
integer x = 2725
integer y = 584
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_reetidet
integer x = 91
integer y = 104
integer width = 2496
integer height = 888
string dataobject = "dw_mant_reetidet"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_Columna, ls_Null
Integer	li_Null
Long		ll_Null

SetNull(ls_Null)
SetNull(li_Null)

ls_Columna = dwo.name

CHOOSE CASE ls_Columna

	CASE "paen_numero"
		IF NoExistePallet(Long(data)) OR Duplicado(Long(data)) THEN
			dw_1.SetItem(il_fila, ls_Columna, Long(ls_Null))
			dw_1.SetItem(il_fila, "espe_codigo", li_Null)
			dw_1.SetItem(il_fila, "vari_codigo", li_Null)
			dw_1.SetItem(il_fila, "vari_nombre", ls_Null)
			dw_1.SetItem(il_fila, "paen_tipopa", li_Null)
			dw_1.SetItem(il_fila, "emba_codigo", ls_Null)
			dw_1.SetItem(il_fila, "cate_codigo", li_Null)
			dw_1.SetItem(il_fila, "palletencab_paen_ccajas", li_Null)
			dw_1.SetItem(il_fila, "etiq_numant", li_Null)
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

type cb_1 from commandbutton within w_mant_deta_reetidet
integer x = 2725
integer y = 868
integer width = 302
integer height = 108
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
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


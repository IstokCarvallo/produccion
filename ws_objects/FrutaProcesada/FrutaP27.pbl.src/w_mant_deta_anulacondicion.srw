$PBExportHeader$w_mant_deta_anulacondicion.srw
forward
global type w_mant_deta_anulacondicion from w_mant_detalle_csd
end type
type cb_1 from commandbutton within w_mant_deta_anulacondicion
end type
end forward

global type w_mant_deta_anulacondicion from w_mant_detalle_csd
integer width = 3154
integer height = 1120
cb_1 cb_1
end type
global w_mant_deta_anulacondicion w_mant_deta_anulacondicion

type variables
Integer ii_estado, ii_condicion
Long  il_fumigacion
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
	END IF
	
	dw_1.Object.paen_numero[il_fila] = Long(istr_mant.Argumento[2])
ELSE
	istr_busq.argum[8]	= String(ii_condicion)
	dw_1.SetColumn("paen_numero")
	dw_1.SetFocus()
END IF

dw_1.Modify("buscapallet.border = 0")
dw_1.Modify("buscapallet.border = 6")

RETURN
end subroutine

public function boolean duplicado (long al_numero);Long		ll_fila
Integer	li_cliente, li_planta, li_cantid

ll_fila	=	dw_1.Find("clie_codigo = " + istr_mant.Argumento[3] + &
						" AND plde_codigo = " + istr_mant.Argumento[1] + &
						" AND paen_numero = " + String(al_Numero) + &
						"AND sage_numero = " + istr_mant.Argumento[10] , 1, dw_1.RowCount())
						
						//" AND sage_numero = " + istr_mant.Argumento[10] + &
	
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Pallet ya fue incluido en Detalle de Anulación", Information!, Ok!)
	RETURN True
END IF

RETURN False
end function

public function boolean noexistepallet (long al_numero);String	ls_nomvar, ls_embala
Integer	li_catego, li_cliente, li_ContCalidad, li_tipopa, li_cajas, li_inspec, &
			li_Destino, li_Especie, li_Variedad, li_planta

li_cliente	= 	Integer(istr_mant.argumento[3])
li_planta	=	Integer(istr_mant.argumento[1])

SELECT	pae.paen_tipopa, var.vari_nombre, pae.emba_codigo, pae.cate_codigo,
			pae.paen_concal, pae.paen_ccajas, pae.paen_inspec, pae.dest_codigo,
			pae.espe_codigo, pae.vari_codigo, pae.paen_estado, isnull(cond_codigo,0)
	INTO	:li_tipopa, :ls_nomvar, :ls_embala, :li_catego, :li_ContCalidad, :li_cajas,
			:li_inspec, :li_Destino, :li_Especie, :li_Variedad, :ii_estado, :ii_condicion
	FROM	dbo.palletencab as pae, dbo.variedades as var
	WHERE pae.clie_codigo	= 	:li_cliente
	AND	pae.paen_numero	= 	:al_numero
	AND	pae.plde_codigo	=	:li_planta
	AND	var.espe_codigo	= 	pae.espe_codigo
	AND	var.vari_codigo	= 	pae.vari_codigo;
	
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
ELSEIF ii_estado = 3 THEN
	MessageBox("Atención", "Pallet fue Repalletizado en Planta Despachadora.", &
			Exclamation!, OK!)
	RETURN True
END IF

IF ii_condicion = 0 THEN
	MessageBox("Atención", "Pallet No tiene Condición.", &
			Exclamation!, OK!)
	RETURN True
END IF


SELECT Distinct fumi_numero 
INTO :il_fumigacion
FROM dbo.fumigadet 	
WHERE paen_numero = :al_numero
AND	clie_codigo = :li_cliente
AND	plde_codigo = :li_planta
AND	cond_codigo = :ii_condicion;

IF il_fumigacion = 0 THEN
	MessageBox("Atención", "Pallet No tiene Fumigación.", &
			Exclamation!, OK!)
	RETURN True
END IF

RETURN False
end function

on w_mant_deta_anulacondicion.create
int iCurrent
call super::create
this.cb_1=create cb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_1
end on

on w_mant_deta_anulacondicion.destroy
call super::destroy
destroy(this.cb_1)
end on

event ue_recuperadatos;call super::ue_recuperadatos;//dw_1.modify("buscapallet.Visible=1")

ias_campo[1]	=	String(dw_1.Object.paen_numero[il_fila])

IF istr_mant.agrega THEN
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))
	dw_1.SetItem(il_fila, "fumi_numero", Long(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "cond_codigo", Integer(istr_mant.argumento[9]))
	dw_1.SetItem(il_fila, "sage_numero", Long(istr_mant.argumento[10]))
END IF

IF istr_mant.agrega = False and istr_mant.borra = False THEN
		dw_1.SetTabOrder("paen_numero", 0)
		dw_1.Modify("paen_numero.BackGround.Color = " + String(RGB(166,180,210)))
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

wf_nuevo()
dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "fumi_numero", Long(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))
dw_1.SetItem(il_fila, "cond_codigo", Integer(istr_mant.argumento[9]))

dw_1.SetColumn("paen_numero")
dw_1.SetFocus()
end event

event open;call super::open;/*
	Argumentos	:	[1]	=	Código de Planta
						[2]	=	Número de Folio Despacho
						[3]	=	Código de Cliente
						[4]	=	Cantidad de Tarjas
						[5]	=	Código de Embarque
						[7]	=	Código de Destino
						[8]	=	Cantidad de Cajas
						[9]	=	Código de Condición
*/


end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_anulacondicion
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_anulacondicion
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_anulacondicion
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_anulacondicion
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_anulacondicion
integer x = 2711
integer y = 368
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_anulacondicion
integer x = 2711
integer y = 152
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_anulacondicion
integer x = 2711
integer y = 584
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_anulacondicion
integer x = 55
integer width = 2551
integer height = 704
string dataobject = "dw_mant_deta_anulacondicion"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_Nula
Integer	li_cliente,pallet,li_status,li_tipo

SetNull(ls_Nula)

ls_columna = dwo.name

CHOOSE CASE ls_columna
			
	CASE "paen_numero"
		IF NoExistePallet(Long(data)) OR Duplicado(Long(data)) THEN
			dw_1.SetItem(il_fila, ls_columna, Long(ls_Nula))
			RETURN 1
		ELSE
			dw_1.Object.cond_codigo[il_fila] = ii_condicion
			dw_1.Object.fumi_numero[il_fila] = il_fumigacion
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

type cb_1 from commandbutton within w_mant_deta_anulacondicion
boolean visible = false
integer x = 2711
integer y = 844
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
boolean enabled = false
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


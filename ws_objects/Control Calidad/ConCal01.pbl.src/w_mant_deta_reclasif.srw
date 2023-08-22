$PBExportHeader$w_mant_deta_reclasif.srw
$PBExportComments$Ventana Mantención Detalle de Reetiquetado.
forward
global type w_mant_deta_reclasif from w_mant_detalle_csd
end type
type cb_1 from commandbutton within w_mant_deta_reclasif
end type
end forward

global type w_mant_deta_reclasif from w_mant_detalle_csd
integer width = 2679
integer height = 1084
cb_1 cb_1
end type
global w_mant_deta_reclasif w_mant_deta_reclasif

type variables
DataWindowChild	 idwc_planta, idwc_emba, idwc_calibr
Integer				ii_estado

end variables

forward prototypes
public function boolean duplicado (long al_numero)
public subroutine buscaplanilla ()
public function boolean noexisteplanilla (long al_numero)
public function boolean noexistecalibre (integer cliente, string calibre)
public function boolean noexisteembalaje (integer cliente, string emba)
end prototypes

public function boolean duplicado (long al_numero);Long		ll_Fila
Integer	li_Cliente, li_Planta

ll_Fila	=	dw_1.Find("clie_codigo = " + istr_mant.Argumento[3] + &
					  " AND plde_codigo = " + istr_mant.Argumento[1] + &
					  " AND recl_numero = " + istr_mant.Argumento[2] + &
					  " AND recl_secuen = " + String(al_Numero), 1, dw_1.RowCount())

IF ll_Fila > 0 and ll_Fila <> il_Fila THEN
	MessageBox("Error","Secuencia ya fue incluida en Detalle de Reclasificado", Information!, Ok!)
	RETURN True
END IF

RETURN False
end function

public subroutine buscaplanilla ();dw_1.Modify("buscaplanilla.border = 0")
dw_1.Modify("buscaplanilla.border = 5")

istr_busq.argum[5]=""
istr_busq.argum[1]	=	String(dw_1.Object.clie_codigo[il_fila])
istr_busq.argum[2]	=	String(dw_1.Object.plde_codigo[il_fila])

OpenWithParm(w_busc_planillasrecl, istr_busq)

istr_busq  =	Message.PowerObjectParm

IF istr_busq.argum[5] <> ""	THEN
	dw_1.setitem(il_fila,"cclo_numero",Long(istr_busq.argum[3]))//lote
	dw_1.setitem(il_fila,"recl_numpla",Long(istr_busq.argum[5]))//planilla
	dw_1.setitem(il_fila,"rcld_calant",istr_busq.argum[6])//calibre ant
	dw_1.setitem(il_fila,"rcld_embant",istr_busq.argum[7])//ebalage ant
	
	dw_1.GetChild("rcld_calibr",idwc_calibr)
	idwc_calibr.SetTransObject(sqlca)
	idwc_calibr.Retrieve(dw_1.Object.clie_codigo[il_fila])
END IF

dw_1.Modify("buscapallet.border = 0")
dw_1.Modify("buscapallet.border = 6")

RETURN
end subroutine

public function boolean noexisteplanilla (long al_numero);String	ls_embala , ls_clibr
Integer	li_cliente, li_planta, li_secuen, li_vari
Long 		ll_planilla, ll_null,ll_lote 
Boolean 	lb_noexiste

SetNull(ll_null)

li_cliente	= 	Integer(istr_mant.argumento[3])
li_planta	=	Integer(istr_mant.argumento[1])
ll_planilla = al_numero

SELECT obj.cctd_folpla, lot.emba_codigo,   
		 lot.vaca_calibr, obj.cctd_secuen, lot.cclo_numero, lot.vari_codigo
INTO  :ll_planilla, :ls_embala,:ls_clibr,
		:li_secuen,:ll_lote, :li_vari
FROM  dbo.ctlcalotesobjetadosdet as obj, dbo.ctlcallotes as lot
WHERE  lot.clie_codigo = obj.clie_codigo
AND    lot.plde_codigo = obj.plde_codigo 
AND    lot.cclo_numero = obj.cclo_numero
AND    obj.cctd_folpla = :ll_planilla
AND    obj.clie_codigo = :li_cliente  
AND    obj.plde_codigo = :li_planta
AND    obj.cctd_resolu = 'R'
AND    (obj.cctd_reclas <> ''
OR     obj.cctd_reclae <> '');
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Palletencab")
	lb_noexiste = true
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de planilla no ha sido creado, Ingrese otro Código.", &
			Exclamation!, OK!)
	lb_noexiste = true	
ELSEIF duplicado(li_secuen) = FALSE	 THEN
	dw_1.SetItem(il_fila, "cclo_numero", ll_lote)
	dw_1.SetItem(il_fila, "rcld_calant", ls_clibr)
	dw_1.SetItem(il_fila, "rcld_embant", ls_embala)
		
	lb_noexiste = FALSE
END IF

RETURN lb_noexiste
end function

public function boolean noexistecalibre (integer cliente, string calibre);//
Integer li_existe
Boolean lb_noexiste

	SELECT	Count(*)
		INTO  :li_existe
		FROM  dbo.variecalibre
		WHERE espe_codigo = 11
		AND   vaca_calibr = :calibre;	

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Variecalibre")
	lb_noexiste = TRUE
ELSEIF li_existe > 0 THEN 
	lb_noexiste = FALSE
ELSE
	messagebox("Error", "No Existe calibre")
	lb_noexiste = TRUE
END IF

RETURN lb_noexiste
end function

public function boolean noexisteembalaje (integer cliente, string emba);integer li_existe
boolean lb_noexiste
string ls_emba

SELECT    count(*),emba_codigo
	INTO   :li_existe, :ls_emba
	FROM   dbo.embalajesprod
	WHERE  emba_codigo = :emba
	AND    clie_codigo = :cliente	
	GROUP BY emba_codigo;
		
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Embalajes")
	lb_noexiste = TRUE
ELSEIF li_existe > 0 AND left(ls_emba,1)= "U" THEN	
	lb_noexiste = FALSE
ELSE
	messagebox("Error", "No Existe Embalaje")	
	lb_noexiste = TRUE
END IF

RETURN lb_noexiste
end function

on w_mant_deta_reclasif.create
int iCurrent
call super::create
this.cb_1=create cb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_1
end on

on w_mant_deta_reclasif.destroy
call super::destroy
destroy(this.cb_1)
end on

event ue_recuperadatos;call super::ue_recuperadatos;dw_1.modify("buscaplanilla.Visible=1")
ias_campo[1] = String(dw_1.Object.recl_numpla[il_Fila])
ias_campo[2] = dw_1.Object.rcld_calibr[il_Fila]
ias_campo[3] = dw_1.Object.emba_codigo[il_Fila]
ias_campo[4] = String(dw_1.Object.cclo_numero[il_Fila])
ias_campo[5] = dw_1.Object.rcld_calant[il_Fila]
ias_campo[6] = dw_1.Object.rcld_embant[il_Fila]



IF istr_mant.agrega THEN
	dw_1.SetItem(il_fila, "recl_numero", Long(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))	
	dw_1.setitem(il_fila,"recl_horare", Now())
END IF

/*
IF istr_mant.agrega = False and istr_mant.borra = False THEN
	dw_1.SetTabOrder("recl_numpla", 0)
	dw_1.modify("recl_numpla.Visible=0")
END IF
*/
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "recl_numpla", Long(ias_campo[1]))
	dw_1.SetItem(il_fila, "rcld_calibr", ias_campo[2])
	dw_1.SetItem(il_fila, "emba_codigo", ias_campo[3])
	dw_1.SetItem(il_fila, "cclo_numero", ias_campo[4])
	dw_1.SetItem(il_fila, "rcld_calant", ias_campo[5])
	dw_1.SetItem(il_fila, "rcld_embant", ias_campo[6])
END IF

end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF IsNull(dw_1.Object.cclo_numero[il_fila]) OR dw_1.Object.cclo_numero[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNúmero de lote"
	ls_colu[li_cont]	= "cclo_numero"
END IF

IF IsNull(dw_1.Object.recl_numpla[il_fila]) OR dw_1.Object.recl_numpla[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nPlanilla"
	ls_colu[li_cont]	= "recl_numpla"
END IF

IF IsNull(dw_1.Object.rcld_calibr[il_fila]) OR dw_1.Object.rcld_calibr[il_fila] = "" THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCalibre"
	ls_colu[li_cont]	= "rcld_calibr"
END IF

IF IsNull(dw_1.Object.emba_codigo[il_fila]) OR dw_1.Object.emba_codigo[il_fila] = "" THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nEmbalaje"
	ls_colu[li_cont]	= "emba_codigo"
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

dw_1.SetItem(il_fila, "recl_numero", Long(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))
dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))


dw_1.SetColumn("cclo_numero")
dw_1.SetFocus()
end event

event open;x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono
istr_mant = Message.PowerObjectParm

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)

dw_1.GetChild("emba_codigo", idwc_emba)
idwc_emba.SetTransObject(sqlca)
idwc_emba.Retrieve(integer(istr_mant.argumento[3]))
idwc_emba.SetFilter("left(emba_codigo,1)= 'U'" )
idwc_emba.Filter()

dw_1.GetChild("rcld_calibr",idwc_calibr)
idwc_calibr.SetTransObject(sqlca)
idwc_calibr.Retrieve(integer(istr_mant.argumento[3]))

dw_1.GetChild("plde_codigo",idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(integer(istr_mant.argumento[3]))

PostEvent("ue_recuperadatos")
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_reclasif
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_reclasif
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_reclasif
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_reclasif
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_reclasif
integer x = 2363
integer y = 368
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_reclasif
integer x = 2363
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

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_reclasif
integer x = 2363
integer y = 584
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_reclasif
integer x = 78
integer height = 716
string dataobject = "dw_mant_deta_reclasif"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_Columna, ls_Null
Integer	li_Null
Long		ll_Null

SetNull(ls_Null)
SetNull(li_Null)

ls_Columna = dwo.name

CHOOSE CASE ls_Columna
	CASE "recl_numpla"
		IF NoExistePlanilla(Long(data)) THEN
			dw_1.SetItem(il_fila, ls_Columna, Long(ls_Null))
			dw_1.SetItem(il_fila, "cclo_numero", li_Null)
			dw_1.SetItem(il_fila, "recl_secuen", li_Null)
			dw_1.SetItem(il_fila, "rcld_calibr", ls_Null)
			dw_1.SetItem(il_fila, "emba_codigo", ls_Null)			
			RETURN 1
		END IF
   CASE "rcld_calibr"
	if noexistecalibre(dw_1.Object.clie_codigo[il_fila], data) then
			dw_1.setitem(il_fila,"rcld_calibr",ls_null)
		end if	
		
	CASE "emba_codigo" 
		if noexisteembalaje(dw_1.Object.clie_codigo[il_fila],data) then
			dw_1.setitem(il_fila,"emba_codigo",ls_null)
		end if	
		
END CHOOSE

end event

event dw_1::buttonclicked;call super::buttonclicked;String	ls_columna

ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "buscaplanilla"
		Buscaplanilla()
		
END CHOOSE
end event

type cb_1 from commandbutton within w_mant_deta_reclasif
boolean visible = false
integer x = 2665
integer y = 816
integer width = 274
integer height = 108
integer taborder = 50
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
		//OpenWithParm(w_maed_palletencab_consulta, lstr_mant)
	END IF
END IF
end event


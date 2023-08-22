$PBExportHeader$w_mant_deta_despafrigode_usda.srw
forward
global type w_mant_deta_despafrigode_usda from w_mant_detalle
end type
type cb_1 from commandbutton within w_mant_deta_despafrigode_usda
end type
type dw_3 from datawindow within w_mant_deta_despafrigode_usda
end type
type cb_ordena from commandbutton within w_mant_deta_despafrigode_usda
end type
type cb_2 from commandbutton within w_mant_deta_despafrigode_usda
end type
type cb_3 from commandbutton within w_mant_deta_despafrigode_usda
end type
end forward

global type w_mant_deta_despafrigode_usda from w_mant_detalle
integer width = 3479
integer height = 1820
cb_1 cb_1
dw_3 dw_3
cb_ordena cb_ordena
cb_2 cb_2
cb_3 cb_3
end type
global w_mant_deta_despafrigode_usda w_mant_deta_despafrigode_usda

type variables
Integer ii_estado
end variables

forward prototypes
public function boolean duplicado (long al_numero)
public function boolean noexistepallet (long al_numero)
public subroutine buscapallet ()
public subroutine wf_selecciona (string as_tecla, long al_fila)
public subroutine wf_nuevo ()
public function boolean wf_actualiza_db (boolean borrando)
public function boolean actualiza_cajas ()
end prototypes

public function boolean duplicado (long al_numero);Long		ll_fila
Integer	li_cliente, li_planta, li_cantid

ll_fila	=	istr_mant.dw.Find("clie_codigo = " + istr_mant.Argumento[3] + &
						" AND plde_codigo = " + istr_mant.Argumento[1] + &
						" AND paen_numero = " + String(al_Numero) , 1, istr_mant.dw.RowCount())
	
IF ll_fila > 0 and ll_fila <> il_filaAnc THEN
	MessageBox("Error","Pallet ya fue incluido en Detalle de Embarque", Information!, Ok!)
	RETURN True
END IF

RETURN False
end function

public function boolean noexistepallet (long al_numero);String	ls_nomvar, ls_embala
Integer	li_catego, li_cliente, li_ContCalidad, li_tipopa, li_cajas, li_inspec, &
			li_Destino, li_Especie, li_Variedad, li_planta, li_fumiga, li_vencimiento
Date		ld_fechainpe			

li_cliente	= 	Integer(istr_mant.argumento[3])
li_planta	=	Integer(istr_mant.argumento[1])

SELECT	pae.paen_tipopa, var.vari_nombre, pae.emba_codigo, pae.cate_codigo,
			pae.paen_concal, pae.paen_ccajas, pae.paen_inspec, pae.dest_codigo,
			pae.espe_codigo, pae.vari_codigo, pae.paen_estado, pae.cond_codigo,
			isnull(var.vari_vencim,0)
	INTO	:li_tipopa, :ls_nomvar, :ls_embala, :li_catego, :li_ContCalidad, :li_cajas,
			:li_inspec, :li_Destino, :li_Especie, :li_Variedad, :ii_estado, :li_fumiga,
			:li_vencimiento
	FROM	dba.palletencab as pae, dba.variedades as var
	WHERE pae.clie_codigo	= 	:li_cliente
	AND	pae.paen_numero	= 	:al_numero
	AND	pae.plde_codigo	=	:li_planta
	AND	var.espe_codigo	= 	pae.espe_codigo
	AND	var.vari_codigo	= 	pae.vari_codigo ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Palletencab")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Pallet no ha sido creado, Ingrese otro Código.", &
			Exclamation!, OK!)
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
			"No Cumple Condición de Embarque", Exclamation!, OK!)
	RETURN True
END IF

/*	Avisa si se embarca un Pucho	*/
IF li_tipopa = 2 THEN
	IF MessageBox("Advertencia", "Se está embarcando un Pucho.~r~r" + &
			"Desea continuar ?", Question!, YesNo!, 2) = 2 THEN RETURN True
END IF

/*	Avisa si se embarca un Pallet Rechazado por SAG	*/
IF li_inspec = 3 THEN
	IF MessageBox("Advertencia", "Se está embarcando un Pallet Rechazado por el SAG.~r~r" + &
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

/*	Verifica Condición de Inspección	*/
IF ((IsNull(li_inspec) OR li_inspec = 0) AND &
(IsNull(li_fumiga) OR li_fumiga = 0) ) AND &
(Integer(istr_mant.argumento[27])=7 OR Integer(istr_mant.argumento[27])=8) AND &
(Integer(istr_mant.argumento[7]) <> 225) THEN
	IF MessageBox("ATENCION", "SE ESTA EMBARCANDO UN FOLIO NO INSPECCIONADO (o NO FUMIGADO)." + &
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
	FROM DBA.inspecpaldet
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
dw_1.SetItem(il_fila, "palletencab_cate_codigo", li_catego)
dw_1.SetItem(il_fila, "palletencab_paen_concal", li_ContCalidad)
dw_1.SetItem(il_fila, "palletencab_paen_ccajas", li_cajas)
dw_1.SetItem(il_fila, "palletencab_paen_inspec", li_inspec)
dw_1.SetItem(il_fila, "palletencab_dest_codigo", li_Destino)

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

public subroutine wf_selecciona (string as_tecla, long al_fila);/*
Función que Administra la Selección de Filas
Argumentos :	as_tecla		Tecla que acompaña el Click
					al_fila		Fila donde se hizo el Click
*/
Long		ll_Filas, ll_FilaIni, ll_FilaSel
Integer	li_Increm

dw_3.SetRedraw(False)

CHOOSE CASE as_tecla
	CASE "Shift"
		ll_filaini	=	dw_3.GetSelectedRow(0)
		
		IF ll_filaini = 0 THEN
			dw_3.SelectRow(al_fila, Not dw_3.IsSelected(al_fila))
		ELSE
			li_increm	=	Sign(al_fila - ll_filaini)
			
			IF li_Increm < 0 THEN
				ll_filaini 	+=	li_increm
			
				DO WHILE ll_filaini <> al_fila + li_increm
					dw_3.SelectRow(ll_filaini, Not dw_3.IsSelected(al_fila))
					ll_filaini 	+=	li_increm
				LOOP
			ELSE
				DO WHILE ll_FilaIni > 0
					ll_FilaSel	=	ll_FilaIni
					ll_FilaIni	=	dw_3.GetSelectedRow(ll_FilaIni + 1)
				LOOP
				
				ll_FilaIni 	=	ll_FilaSel + 1
			
				DO WHILE ll_FilaIni <> al_fila + 1
					dw_3.SelectRow(ll_FilaIni, Not dw_3.IsSelected(al_fila))
					ll_FilaIni 	++
				LOOP
			END IF
		END IF
		
	CASE "Control"
		IF al_fila > 0 THEN dw_3.SelectRow(al_fila, Not dw_3.IsSelected(al_fila))
		
	CASE ELSE
		dw_3.SelectRow(0, False)
		dw_3.SelectRow(al_fila, True)
			
END CHOOSE

dw_3.SetRedraw(True)
end subroutine

public subroutine wf_nuevo ();il_fila = dw_1.InsertRow(0)

dw_1.SetRedraw(False)
dw_1.SetRow(il_fila)
dw_1.ScrollToRow(il_fila)
istr_mant.dw.SetRow(il_fila)
istr_mant.dw.ScrolltoRow(il_fila)
istr_mant.dw.SelectRow(0,False)
istr_mant.dw.SelectRow(il_fila,True)
dw_1.SetRedraw(True)


end subroutine

public function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_3.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_3.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE

	IF dw_3.Update(True, False) = 1 THEN
		
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		ELSE
			lb_Retorno	=	True
		
			dw_3.ResetUpdate()
		END IF
		
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	END IF
	

END IF

//actualiza_cajas()

dw_3.Reset()

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean actualiza_cajas ();String	ls_nomvar, ls_embala
Integer	li_cliente, li_planta
Long		ll_numero, ll_cajas

li_cliente	= 	Integer(istr_mant.argumento[3])
li_planta	=	Integer(istr_mant.argumento[1])
ll_numero	=	Long(dw_1.Object.paen_numero[1])
ll_cajas		=	Long(dw_3.Object.disponible[1])


UPDATE dba.palletencab SET
paen_ccajas = :ll_cajas
WHERE paen_numero = :ll_numero 
AND	plde_codigo = :li_planta
AND	clie_codigo = :li_cliente;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Palletencab")
	RETURN True
END IF

Return False


end function

on w_mant_deta_despafrigode_usda.create
int iCurrent
call super::create
this.cb_1=create cb_1
this.dw_3=create dw_3
this.cb_ordena=create cb_ordena
this.cb_2=create cb_2
this.cb_3=create cb_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_1
this.Control[iCurrent+2]=this.dw_3
this.Control[iCurrent+3]=this.cb_ordena
this.Control[iCurrent+4]=this.cb_2
this.Control[iCurrent+5]=this.cb_3
end on

on w_mant_deta_despafrigode_usda.destroy
call super::destroy
destroy(this.cb_1)
destroy(this.dw_3)
destroy(this.cb_ordena)
destroy(this.cb_2)
destroy(this.cb_3)
end on

event ue_recuperadatos;call super::ue_recuperadatos;dw_1.modify("buscapallet.Visible=1")

ias_campo[1]	=	String(dw_1.Object.paen_numero[il_fila])
ias_campo[2]	=	String(dw_1.Object.defe_termog[il_fila])

IF istr_mant.agrega THEN
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))
	dw_1.SetItem(il_fila, "defe_numero", Long(istr_mant.argumento[2]))
END IF

IF istr_mant.agrega = False and istr_mant.borra = False THEN
		dw_1.SetTabOrder("paen_numero", 0)
		dw_1.Modify("paen_numero.BackGround.Color = " + String(RGB(166,180,210)))
		dw_1.modify("buscapallet.Visible=0")
END IF

end event

event ue_deshace;call super::ue_deshace;dw_1.Object.paen_numero[il_fila]	=	Long(ias_campo[1])
dw_1.Object.defe_termog[il_fila]	=	Char(ias_campo[2])
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

//IF dw_1.RowCount() >= Integer(istr_mant.Argumento[4]) THEN
//	MessageBox("Atención", "No puede ingresar más Pallets.")
//	
//	pb_salir.TriggerEvent(Clicked!)
//ELSEIF dw_1.Object.totcajas[1] >= Integer(istr_mant.Argumento[8]) THEN
//	MessageBox("Atención", "Se ha completado la cantidad de Cajas, No podrá ingresar más Pallets.")
//	
//	pb_salir.TriggerEvent(Clicked!)
//ELSE
	wf_nuevo()
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "defe_numero", Long(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))

	dw_1.SetColumn("paen_numero")
	dw_1.SetFocus()
//END IF
end event

event open;call super::open;/*
	Argumentos	:	[1]	=	Código de Planta
						[2]	=	Número de Folio Despacho
						[3]	=	Código de Cliente
						[4]	=	Cantidad de Tarjas
						[5]	=	Código de Embarque
						[7]	=	Código de Destino
						[8]	=	Cantidad de Cajas
						[20]	=	Tipo salida
*/

dw_3.SetTransObject(sqlca)


end event

event ue_guardar;IF dw_3.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_despafrigode_usda
boolean visible = false
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_despafrigode_usda
boolean visible = false
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_despafrigode_usda
boolean visible = false
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_despafrigode_usda
boolean visible = false
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_despafrigode_usda
integer x = 3173
integer y = 368
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_despafrigode_usda
integer x = 3173
integer y = 152
end type

event pb_acepta::clicked;istr_mant.respuesta = 1

IF istr_mant.agrega THEN
//	IF (dw_1.Rowcount()<Integer(istr_mant.argumento[4])) THEN
//		IF dw_1.object.totcajas[1] < Integer(istr_mant.argumento[8]) THEN
//			Parent.TriggerEvent("ue_nuevo")
//		ELSE
//			MessageBox("               ADVERTENCIA","Se ha completado la cantidad de CAJAS~r" + &
//							"indicados en el Despacho.~r~rSe Retornará a la Ventana de Mantención.", &
//							Information!, OK!)
//		  CloseWithReturn(Parent, istr_mant)
//		END IF
//	ELSE
//		MessageBox("               ADVERTENCIA","Se ha completado la cantidad de Pallets~r" + &
//							"indicados en el Despacho.~r~rSe Retornará a la Ventana de Mantención.", &
//							Information!, OK!)
//		CloseWithReturn(Parent, istr_mant)
//	END IF
//ELSE
	//CloseWithReturn(Parent, istr_mant)
END IF

istr_mant.respuesta = 1

IF istr_mant.agrega THEN
	Parent.TriggerEvent("ue_nuevo")
ELSE
	CloseWithReturn(Parent, istr_mant)
END IF
end event

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_despafrigode_usda
integer x = 3173
integer y = 584
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_despafrigode_usda
integer x = 0
integer y = 0
integer width = 2971
integer height = 488
string dataobject = "dw_mant_despafrigode_usda"
boolean ib_allow_updates = false
boolean ib_allow_inserts = false
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
			dw_3.Retrieve(Integer(istr_mant.argumento[3]),Long(data),Integer(istr_mant.argumento[1]))
			IF dw_3.RowCount() > 0 THEN
				cb_2.Enabled 		= True
				cb_3.Enabled 		= True
				cb_ordena.Enabled = True
			ELSE
				cb_2.Enabled 		= False
				cb_3.Enabled 		= False
				cb_ordena.Enabled	= False
			END IF	
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

type cb_1 from commandbutton within w_mant_deta_despafrigode_usda
boolean visible = false
integer x = 3118
integer y = 816
integer width = 274
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

type dw_3 from datawindow within w_mant_deta_despafrigode_usda
integer x = 5
integer y = 580
integer width = 2990
integer height = 1036
integer taborder = 90
boolean bringtotop = true
string dataobject = "dw_mues_palletfruta_usda"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;String	ls_Tecla, ls_Columna, ls_Orden[] = {" A", " D"}, &
			ls_Columnas[]	=	{"oper_codigo", "embq_codigo", "espe_codigo", &
									"vari_codigo", "vari_nombre", "zona_codigo", &
									"prod_codigo", "cate_codigo", "pool_calibr", &
									"cond_codigo", "enva_tipoen", "enva_codigo", &
									"etiq_codigo"}, &
			ls_Ordenam		=	"&oper_codigo_t01&embq_codigo_t02&espe_codigo_t03&vari_codigo_t04" + &
									"&vari_nombre_t05&zona_codigo_t06&prod_codigo_t07&cate_codigo_t08" + &
									"&pool_calibr_t09&cond_codigo_t10&enva_tipoen_t11&enva_codigo_t12&etiq_codigo_t13"
									
Long		ll_Fila, ll_Anexo
Integer	li_Posicion

IF Row > 0 THEN
   il_Fila	=	Row

	IF KeyDown(KeyShift!) THEN
		ls_tecla	=	"Shift"
	ELSEIF KeyDown(KeyControl!) THEN
		ls_tecla	=	"Control"
	END IF
	
	wf_Selecciona(ls_Tecla, Row)
END IF
end event

event itemchanged;String	ls_Nula, ls_Columna

SetNull(ls_Nula)

ls_Columna	= dwo.name

CHOOSE CASE ls_Columna
	CASE "cajventa"
		IF	Long(data) > dw_3.Object.cajas[row] THEN
			MessageBox("Cuidado!","No Tiene Cajas Disponible para ese Registro")
			dw_3.SetItem(row, ls_Columna, Long(ls_Nula))			
			RETURN 1
		ELSE
			dw_3.Object.cajas[row] = dw_3.Object.cajas[row] - Long(data)
			dw_3.Object.PAFR_CJSSAL[row] = integer(data) + dw_3.Object.PAFR_CJSSAL[row]
			dw_3.Object.pafr_barra3[row] = istr_mant.argumento[25]
		END IF	
		
	
		
END CHOOSE
end event

type cb_ordena from commandbutton within w_mant_deta_despafrigode_usda
integer x = 1289
integer y = 1632
integer width = 402
integer height = 72
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Ordenar"
end type

event clicked;String	ls_Retorno

str_parms	lstr_Parm

lstr_Parm.String_arg[1]	=	"Cod. Productor:prod_codigo," + &
									"Productor:prod_nombre," + &
									"Embalaje:emba_codigo," + &
									"Fecha Embalaje:pafr_fecemb," + &
									"Nombre Variedad:vari_nombre," + &
									"Calibre:pafr_calibr," + &
									"Lote:pafr_nrlote," + &
									"Fecha Ingreso:pafr_fecing"
lstr_Parm.dw_arg			=	dw_3

OpenWithParm(w_columna_orden, lstr_Parm)

ls_Retorno	=	Message.StringParm

dw_3.SetRow(1)

RETURN
end event

type cb_2 from commandbutton within w_mant_deta_despafrigode_usda
integer x = 27
integer y = 500
integer width = 256
integer height = 72
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Todos"
end type

event clicked;Long  ll_fila

dw_3.SelectRow(0, True)


FOR ll_fila = 1 TO dw_3.Rowcount()
	dw_3.Object.cajventa[ll_fila] 	= dw_3.Object.cajas[ll_fila]
	dw_3.Object.PAFR_CJSSAL[ll_fila] = dw_3.Object.cajas[ll_fila] + dw_3.Object.PAFR_CJSSAL[ll_fila]
	dw_3.Object.pafr_barra3[ll_fila] = istr_mant.argumento[25]
NEXT	
end event

type cb_3 from commandbutton within w_mant_deta_despafrigode_usda
integer x = 2715
integer y = 500
integer width = 261
integer height = 72
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Ninguno"
end type

event clicked;Long	ll_pallet

dw_3.SelectRow(0, False)

ll_pallet	=	dw_1.Object.paen_numero[1]

dw_3.Retrieve(Integer(istr_mant.argumento[3]),ll_pallet,Integer(istr_mant.argumento[1]))
end event


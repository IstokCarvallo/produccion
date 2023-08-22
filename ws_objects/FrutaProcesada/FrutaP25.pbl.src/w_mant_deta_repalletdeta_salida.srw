$PBExportHeader$w_mant_deta_repalletdeta_salida.srw
forward
global type w_mant_deta_repalletdeta_salida from w_mant_detalle_csd
end type
type cb_1 from commandbutton within w_mant_deta_repalletdeta_salida
end type
end forward

global type w_mant_deta_repalletdeta_salida from w_mant_detalle_csd
integer width = 2994
integer height = 1176
string title = "Ingreso Pallet"
cb_1 cb_1
end type
global w_mant_deta_repalletdeta_salida w_mant_deta_repalletdeta_salida

type variables
DataWindowChild	dwc_especie, dwc_variedad, dwc_embalaje, dwc_tipopalemb, dwc_calibre
end variables

forward prototypes
public function boolean noexisteendddw (string as_columna, string as_valor, string as_nombre, boolean ab_string)
public subroutine wf_opciontodas ()
public function boolean noexistecalibre (string as_calibre)
public function boolean noexisteembalaje (string as_embalaje)
public function boolean noexistepallet (string ls_columna)
public function boolean existepallet (string ls_columna)
public function boolean duplicado (string campo)
public subroutine habilitacolumna (boolean ab_habilita)
public function boolean duplicado_nuevofolio (string campo)
end prototypes

public function boolean noexisteendddw (string as_columna, string as_valor, string as_nombre, boolean ab_string);DataWindowChild	ldwc_1

String	ls_busca
Long		ll_fila

dw_1.GetChild(as_columna,ldwc_1)

IF ab_string THEN
	ls_busca	=	as_columna + " = '" + as_valor + "'"
ELSE
	ls_busca	=	as_columna + " = " + as_valor
END IF

IF Isnull(ls_busca) THEN
	ll_fila = 1
ELSE
	ll_fila	=	ldwc_1.Find(ls_busca,1,ldwc_1.RowCount())
END IF

IF ll_fila = 0 THEN
	MessageBox("Atención ", as_nombre + " no ha sido ingresado.", Exclamation!, OK!) 
	RETURN True
ELSE
	IF as_nombre = 'Variedad' THEN
		dw_1.SetItem(il_fila,"vari_nombre",ldwc_1.GetItemString(ll_fila,"vari_nombre"))
	ELSEIF as_nombre = 'Tipo de Pallet' THEN
		dw_1.SetItem(il_fila,"tpem_cancaj",ldwc_1.GetItemNumber(ll_fila,"tpem_cancaj"))
	END IF
	RETURN False
END IF
end function

public subroutine wf_opciontodas ();Long		ll_fila
Integer	li_null
String	ls_null

SetNull(li_null)
SetNull(ls_null)

//Inserción de Opción Todas

//ll_fila	= dwc_especie.InsertRow(1)
//dwc_especie.SetItem(ll_fila,"espe_codigo",li_null)
//dwc_especie.SetItem(ll_fila,"espe_nombre","TODAS")
//
//ll_fila	= dwc_variedad.InsertRow(1)
//dwc_variedad.SetItem(ll_fila,"vari_codigo",li_null)
//dwc_variedad.SetItem(ll_fila,"vari_nombre","TODAS")
end subroutine

public function boolean noexistecalibre (string as_calibre);String		ls_nombre
Integer		li_Cliente, li_Especie, li_Variedad

li_Cliente	=	Integer(istr_mant.Argumento[1])
li_Especie	=	dw_1.Object.espe_codigo[il_Fila]
li_Variedad	=	dw_1.Object.vari_codigo[il_Fila]

IF IsNull(li_Especie) = False OR li_Especie > 0 OR &
	IsNull(li_Variedad) = False OR li_Variedad > 0 OR &
	IsNull(as_Calibre) = False OR as_Calibre = '' THEN
	SELECT	vaca_calibr
		INTO	:ls_nombre
		FROM	dba.variecalibre
		WHERE	espe_codigo	=	:li_Especie
		AND	vari_codigo =	:li_Variedad
		AND	vaca_calibr =	:as_Calibre;

	IF sqlca.SQLCode = -1 THEN
  		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla variecalibre")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Calibre no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
		
		RETURN True
	ELSE
		
		RETURN False
	END IF
ELSE
	RETURN True
END IF
end function

public function boolean noexisteembalaje (string as_embalaje);Integer li_fila, li_cliente

li_cliente = Integer(istr_mant.Argumento[1])

IF IsNull(as_embalaje) or Trim(as_embalaje) = "" THEN
	RETURN TRUE
ELSE
	
	SELECT	Count(*)
		INTO	:li_fila
		FROM	dba.embalajesprod
		WHERE	emba_codigo	=	:as_embalaje
		AND	clie_codigo =	:li_cliente;
		
	IF sqlca.SQLCode = -1 THEN
  		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla EmbalajesProd")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Embalaje no ha sido Definido para el cliente.~r~r" + &
					"Ingrese o seleccione otro Código.")
		
		RETURN True
	ELSE
		
		RETURN False
	END IF
END IF

end function

public function boolean noexistepallet (string ls_columna);String	ls_nomvar, ls_embala
Integer	li_catego, li_cliente, li_status, li_tipopa, li_Estado, li_planta, li_varicod
Long		ll_numero, ll_cajas,ll_pcopda
Date     ld_fecha

li_cliente	= 	Integer(istr_mant.argumento[3])
li_planta	=	Integer(istr_mant.argumento[1])
ll_numero 	= 	Long(ls_columna)

SELECT	pae.paen_tipopa, var.vari_nombre, pae.emba_codigo, pae.cate_codigo,
			pae.stat_codigo, pae.paen_estado, pae.paen_ccajas, pae.paen_fecemb,
			pae.vari_codigo, pae.paen_pcopda
	INTO	:li_tipopa, :ls_nomvar, :ls_embala, :li_catego,
			:li_status, :li_Estado, :ll_cajas, :ld_fecha, :li_varicod,
			:ll_pcopda
	FROM	dba.palletencab as pae, dba.variedades as var
	WHERE pae.clie_codigo	= 	:li_cliente
	AND	pae.paen_numero	= 	:ll_numero
	AND	pae.plde_codigo	=	:li_planta
	AND	var.espe_codigo	= 	pae.espe_codigo
	AND	var.vari_codigo	= 	pae.vari_codigo ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Palletencab")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Pallet no ha sido creado, Ingrese otro Código.", &
					Exclamation!, OK!)
	cb_1.Enabled	=	False
	RETURN True
ELSEIF li_Estado = 2 THEN
	MessageBox("Atención","Pallet no Existe, fue Despachado.")
	cb_1.Enabled	=	False
	RETURN True
ELSEIF li_Estado = 3 THEN
	MessageBox("Atención","Pallet no Existe, ya fue Repalletizado.")
	cb_1.Enabled	=	False
	RETURN True
ELSEIF ll_pcopda = 2 OR ll_pcopda	=	3 	OR ll_pcopda	=	4 OR &
 		ll_pcopda	=	6  THEN
		MessageBox("Atención","El Numero de Pallet Ingresado es un Ingreso Caja a Caja. Ingrese Otro.", Exclamation!, Ok!)
		RETURN True
ELSE
	dw_1.SetItem(il_fila, "repd_tipood", 1)
	dw_1.SetItem(il_fila, "paen_numero", ll_numero)
	dw_1.SetItem(il_fila, "vari_nombre", ls_nomvar)
	dw_1.SetItem(il_fila, "paen_tipopa", li_tipopa)
	dw_1.SetItem(il_fila, "emba_codigo", ls_embala)
	dw_1.SetItem(il_fila, "cate_codigo", li_catego)
	dw_1.SetItem(il_fila, "stat_codigo", li_status)
	dw_1.SetItem(il_fila, "paen_ccajas", ll_cajas)
	dw_1.SetItem(il_fila, "vari_codigo", li_varicod)
	dw_1.SetItem(il_fila, "paen_fecemb", ld_fecha)
	dw_1.SetItem(il_fila, "pafr_secuen", 1)
	cb_1.Enabled	=	True
	RETURN False
END IF

end function

public function boolean existepallet (string ls_columna);Integer	li_cliente, li_tipopa, li_planta
Long		ll_numero

li_cliente	= 	Integer(istr_mant.argumento[3])
li_planta	=	Integer(istr_mant.argumento[1])
ll_numero 	= 	Long(ls_columna)

SELECT	pae.paen_tipopa
	INTO	:li_tipopa
	FROM	dba.palletencab as pae
	WHERE pae.clie_codigo	= 	:li_cliente
	AND	pae.paen_numero	= 	:ll_numero
	AND	pae.plde_codigo	=	:li_planta;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Palletencab")
	RETURN True
ELSEIF sqlca.SQLCode = 0 THEN
	MessageBox("Atención","Pallet fue Ingresado Anteriormente.")
	RETURN True	
ELSE
	RETURN False
END IF
	
end function

public function boolean duplicado (string campo);Long		ll_fila

ll_fila	= dw_1.Find("paen_numero = " + campo + " ", 1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Pallet ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine habilitacolumna (boolean ab_habilita);IF ab_habilita	THEN 
	dw_1.Object.repd_neopal.Protect				=	0
	dw_1.Modify("repd_neopal.BackGround.Color	=	" + String(rgb(255,255,255)))
ELSE
	dw_1.Object.repd_neopal.Protect				=	1
	dw_1.Modify("repd_neopal.BackGround.Color	=	" + String(RGB(166,180,210)))
END IF	
end subroutine

public function boolean duplicado_nuevofolio (string campo);Long		ll_fila

ll_fila	= dw_1.Find("repd_neopal = " + campo + " ", 1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Pallet ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

event open;call super::open;dw_1.TabOrder = 20

cb_1.Enabled	=	False
end event

on w_mant_deta_repalletdeta_salida.create
int iCurrent
call super::create
this.cb_1=create cb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_1
end on

on w_mant_deta_repalletdeta_salida.destroy
call super::destroy
destroy(this.cb_1)
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1]	=	String(dw_1.Object.paen_numero[il_fila])

dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "repe_numero", Long(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))

IF istr_mant.argumento[25]	=	'6'	THEN
	habilitacolumna(True)
ELSE
	habilitacolumna(False)
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "repe_numero", Long(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))

IF istr_mant.argumento[25]	=	'6'	THEN
	habilitacolumna(True)
ELSE
	habilitacolumna(False)
END IF

dw_1.SetColumn("paen_numero")

end event

event ue_deshace;call super::ue_deshace;dw_1.Object.paen_numero[il_fila]=	Long(ias_campo[1])
//ias_campo[1]	=	String(dw_1.Object.paen_numero[il_fila])

end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]
long     var

IF Isnull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.paen_numero[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNúmero de Pallet"
	ls_colu[li_cont]	= "paen_numero"
END IF

var=dw_1.Object.repd_neopal[il_fila]

IF (Isnull(dw_1.Object.repd_neopal[il_fila]) OR dw_1.Object.repd_neopal[il_fila] = 0) AND istr_mant.argumento[25] = '6' THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNuevo Número de Pallet"
	ls_colu[li_cont]	= "repd_neopal"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_repalletdeta_salida
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_repalletdeta_salida
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_repalletdeta_salida
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_repalletdeta_salida
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_repalletdeta_salida
integer x = 2711
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_repalletdeta_salida
integer x = 2711
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_repalletdeta_salida
integer x = 2711
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_repalletdeta_salida
integer width = 2491
integer height = 960
string dataobject = "dw_mant_repalletdeta"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_null
Long		ll_null

SetNull(ls_null)
SetNull(ll_null)

ls_columna = dwo.name

CHOOSE CASE ls_columna
	
CASE "paen_numero"
	IF NoExistePallet(data) OR Duplicado(data) THEN
		dw_1.SetItem(il_fila, "paen_numero", ll_null)
		RETURN 1
   END IF
CASE "repd_neopal"
	IF ExistePallet(data) OR Duplicado_NuevoFolio(data) THEN
		dw_1.SetItem(il_fila, "repd_neopal", ll_null)
		RETURN 
	END IF
	pb_acepta.SetFocus()
END CHOOSE
end event

event dw_1::itemerror;RETURN 1
end event

type cb_1 from commandbutton within w_mant_deta_repalletdeta_salida
integer x = 2656
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
string text = "&Detalle"
end type

event clicked;IF dw_1.RowCount() > 0 THEN
	istr_mant.Agrega			=	False
	istr_mant.Borra			=	False
	istr_mant.Argumento[6]	=	String(dw_1.GetitemNumber(dw_1.GetRow(),"paen_numero"))
   istr_mant.Argumento[7]	=	'1'
	istr_mant.Argumento[8]	=	String(dw_1.GetitemNumber(dw_1.GetRow(),"clie_codigo"))
	istr_mant.Argumento[9]	=	String(dw_1.GetitemNumber(dw_1.GetRow(),"espe_codigo"))
	istr_mant.Argumento[10]	=	String(dw_1.GetitemNumber(dw_1.GetRow(),"vari_codigo"))
	istr_mant.Argumento[11]	=	'100'
	istr_mant.Argumento[12]	=	'100'
	
	IF IsNull(istr_mant.Argumento[6]) = False AND istr_mant.Argumento[6] <> "" THEN
		OpenWithParm(w_maed_palletencab_consulta, istr_mant)
	END IF
END IF
end event


$PBExportHeader$w_mant_cambiaalturapacking.srw
$PBExportComments$Genera archivo Plano SAG por Repalletizaciones.
forward
global type w_mant_cambiaalturapacking from window
end type
type dw_1 from datawindow within w_mant_cambiaalturapacking
end type
type st_9 from statictext within w_mant_cambiaalturapacking
end type
type em_embalaje from editmask within w_mant_cambiaalturapacking
end type
type em_altura from editmask within w_mant_cambiaalturapacking
end type
type st_7 from statictext within w_mant_cambiaalturapacking
end type
type em_nueva from editmask within w_mant_cambiaalturapacking
end type
type st_2 from statictext within w_mant_cambiaalturapacking
end type
type em_numero from editmask within w_mant_cambiaalturapacking
end type
type st_numero from statictext within w_mant_cambiaalturapacking
end type
type dw_planta from datawindow within w_mant_cambiaalturapacking
end type
type dw_cliente from datawindow within w_mant_cambiaalturapacking
end type
type st_4 from statictext within w_mant_cambiaalturapacking
end type
type st_3 from statictext within w_mant_cambiaalturapacking
end type
type st_5 from statictext within w_mant_cambiaalturapacking
end type
type st_1 from statictext within w_mant_cambiaalturapacking
end type
type pb_salir from picturebutton within w_mant_cambiaalturapacking
end type
type pb_grabar from picturebutton within w_mant_cambiaalturapacking
end type
type gb_2 from groupbox within w_mant_cambiaalturapacking
end type
type gb_1 from groupbox within w_mant_cambiaalturapacking
end type
type st_8 from statictext within w_mant_cambiaalturapacking
end type
type sle_mensa from singlelineedit within w_mant_cambiaalturapacking
end type
type st_6 from statictext within w_mant_cambiaalturapacking
end type
end forward

global type w_mant_cambiaalturapacking from window
integer width = 2437
integer height = 1156
boolean titlebar = true
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 30586022
string icon = "TABLA.ICO"
event ue_guardar ( )
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
event ue_genera_facturas pbm_custom04
dw_1 dw_1
st_9 st_9
em_embalaje em_embalaje
em_altura em_altura
st_7 st_7
em_nueva em_nueva
st_2 st_2
em_numero em_numero
st_numero st_numero
dw_planta dw_planta
dw_cliente dw_cliente
st_4 st_4
st_3 st_3
st_5 st_5
st_1 st_1
pb_salir pb_salir
pb_grabar pb_grabar
gb_2 gb_2
gb_1 gb_1
st_8 st_8
sle_mensa sle_mensa
st_6 st_6
end type
global w_mant_cambiaalturapacking w_mant_cambiaalturapacking

type variables
str_mant			istr_mant
str_busqueda	istr_busq
String	is_altura, is_embalaje

DataWindowChild	idwc_cliente, idwc_planta


end variables

forward prototypes
public function boolean noexistecliente (integer ai_cliente)
public function integer codigoplantasag (integer planta)
public function boolean noexisteplanta (integer ai_planta)
public function boolean existepalletfrigorifico (long ai_numero)
public function boolean buscapallet (long ai_numero)
public function boolean valida_altura (string as_altura, string as_nuevaaltura, long ai_numero)
public function boolean actualiza_pallet (long ai_numero)
end prototypes

event ue_listo;w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

public function boolean noexistecliente (integer ai_cliente);String	ls_Nombre

SELECT	clie_nombre
	INTO	:ls_Nombre
	FROM	dba.CLIENTESPROD
	WHERE	clie_codigo	=	:ai_Cliente ;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Clientes Producción")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.", Exclamation!, Ok!)
					
	RETURN True
ELSE
	RETURN False
END IF
end function

public function integer codigoplantasag (integer planta);Integer	li_Cliente, li_codigo

li_Cliente	=	Integer(istr_mant.Argumento[1])

SELECT	plde_codpla
	INTO	:li_Codigo
	FROM	dba.plantadesp
	WHERE	plde_codigo	=	:planta;

RETURN li_Codigo

end function

public function boolean noexisteplanta (integer ai_planta);String	ls_Nombre
Integer	li_cliente

li_cliente	=	Integer(istr_mant.argumento[1])

SELECT	plde_nombre
	INTO	:ls_Nombre
	FROM	dba.PLANTADESP
	WHERE	plde_codigo =  :ai_planta;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Plantas y Frigoríficos")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.", Exclamation!, Ok!)
	
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean existepalletfrigorifico (long ai_numero);Long	ll_cont
Integer	li_cliente, li_planta

li_cliente = Integer(istr_mant.argumento[1])
li_planta  = Integer(istr_mant.argumento[2])

SELECT	count()
INTO	:ll_cont
FROM	dba.palletencab
WHERE	clie_codigo	= :li_cliente
AND	plde_codigo = :li_planta
AND	paen_numero = :ai_numero;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla palletencab")
	
	RETURN True
ELSEIF ll_cont > 0 THEN
	MessageBox("Atención", "Pallet Se Encuentra en Frigorífico.~r~r" + &
					"Ingrese o seleccione otro Pallet.", Exclamation!, Ok!)
					
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean buscapallet (long ai_numero);Integer	li_Cliente, li_Planta, li_cont, li_cont2
Long		ll_cont, ll_filas, ll_fila, fila

li_Cliente	=	Integer(istr_mant.Argumento[1])
li_Planta	=	Integer(istr_mant.Argumento[2])
li_cont2		=	0

ll_filas = dw_1.Retrieve(li_Cliente,li_planta,ai_numero)

IF ll_filas = 1 THEN
	is_altura 	= dw_1.Object.tpem_codigo[1]
	is_embalaje	= dw_1.Object.emba_codigo[1]
ELSEIF ll_filas > 1 THEN	
	FOR ll_fila = 1 TO dw_1.RowCount()
		li_cont 	= dw_1.Object.contador[ll_fila]
		IF li_cont2 <= li_cont THEN
			li_cont2 = li_cont
			fila		= ll_fila
		END IF	
	NEXT
	is_altura 	= dw_1.Object.tpem_codigo[fila]
	is_embalaje	= dw_1.Object.emba_codigo[fila]
END IF	

IF ll_filas = 0 THEN
	MessageBox("Atención", "Pallet NO Existe en Packing.~r~r" + &
					"Ingrese o seleccione otro Pallet.", Exclamation!, Ok!)
	RETURN True
END IF

	
RETURN False
end function

public function boolean valida_altura (string as_altura, string as_nuevaaltura, long ai_numero);Long	ll_cont
Integer	li_cliente, li_planta

li_Cliente	=	Integer(istr_mant.Argumento[1])
li_Planta	=	Integer(istr_mant.Argumento[2])

SELECT	Count()
	INTO	:ll_cont
	FROM	dba.spro_palletfruta as pal,dba.tipopallemba as tip,
		dba.spro_palletencab as pan
	WHERE	pal.clie_codigo	= :li_Cliente
	AND	pal.plde_codigo = :li_planta
	AND	pal.paen_numero = :ai_numero
	AND	pal.clie_codigo = pan.clie_codigo
	AND	pal.plde_codigo = pan.plde_codigo
	AND 	pal.paen_numero = pan.paen_numero
	AND	pal.clie_codigo = tip.clie_codigo
	AND	pal.emba_codigo = tip.emba_codigo
	AND	tip.tpem_codigo = :as_nuevaaltura
	AND	:as_altura 		 < :as_nuevaaltura
	AND	isnull(emba_altura,'') <> ''
	AND	isnull(emba_columna,'') <> '';
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_palletfruta")
	
	RETURN True
ELSEIF ll_cont = 0 THEN
	MessageBox("Atención", "Altura NO Existe o Falta Mantención de Tabla.~r~r" + &
					"Revise Tabla Tipo Pallet.", Exclamation!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF	
	Return False

end function

public function boolean actualiza_pallet (long ai_numero);Integer	li_cliente, li_planta
String	ls_altura

li_Cliente	=	Integer(istr_mant.Argumento[1])
li_Planta	=	Integer(istr_mant.Argumento[2])
ls_altura	=  em_nueva.Text

UPDATE dba.spro_Palletfruta SET
tpem_codigo = :ls_altura
WHERE clie_codigo = :li_cliente
AND	plde_codigo = :li_planta
AND	paen_numero = :ai_numero;
Commit;
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_palletfruta")
	Return True
END IF

UPDATE dba.spro_palletencab SET
paen_ccajas = :ls_altura
WHERE clie_codigo = :li_cliente
AND	plde_codigo = :li_planta
AND	paen_numero = :ai_numero;
Commit;
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_palletencab")
	Return True
END IF


sle_mensa.Text = 'Cambio Realizado con Exito'
pb_grabar.Enabled = False
Return False

end function

on w_mant_cambiaalturapacking.create
this.dw_1=create dw_1
this.st_9=create st_9
this.em_embalaje=create em_embalaje
this.em_altura=create em_altura
this.st_7=create st_7
this.em_nueva=create em_nueva
this.st_2=create st_2
this.em_numero=create em_numero
this.st_numero=create st_numero
this.dw_planta=create dw_planta
this.dw_cliente=create dw_cliente
this.st_4=create st_4
this.st_3=create st_3
this.st_5=create st_5
this.st_1=create st_1
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.gb_2=create gb_2
this.gb_1=create gb_1
this.st_8=create st_8
this.sle_mensa=create sle_mensa
this.st_6=create st_6
this.Control[]={this.dw_1,&
this.st_9,&
this.em_embalaje,&
this.em_altura,&
this.st_7,&
this.em_nueva,&
this.st_2,&
this.em_numero,&
this.st_numero,&
this.dw_planta,&
this.dw_cliente,&
this.st_4,&
this.st_3,&
this.st_5,&
this.st_1,&
this.pb_salir,&
this.pb_grabar,&
this.gb_2,&
this.gb_1,&
this.st_8,&
this.sle_mensa,&
this.st_6}
end on

on w_mant_cambiaalturapacking.destroy
destroy(this.dw_1)
destroy(this.st_9)
destroy(this.em_embalaje)
destroy(this.em_altura)
destroy(this.st_7)
destroy(this.em_nueva)
destroy(this.st_2)
destroy(this.em_numero)
destroy(this.st_numero)
destroy(this.dw_planta)
destroy(this.dw_cliente)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.st_1)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.gb_2)
destroy(this.gb_1)
destroy(this.st_8)
destroy(this.sle_mensa)
destroy(this.st_6)
end on

event open;x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

DataWindowChild	ldwc_especie, ldwc_embalaje

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1,"clie_codigo",gi_CodExport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
dw_planta.SetItem(1,"plde_codigo",gi_CodPlanta)

istr_mant.argumento[1]	=	String(gi_CodExport)
istr_mant.argumento[2]	=	String(gi_CodPlanta)

dw_1.SetTransObject(sqlca)







end event

type dw_1 from datawindow within w_mant_cambiaalturapacking
integer x = 1006
integer y = 1244
integer width = 686
integer height = 400
integer taborder = 70
string title = "none"
string dataobject = "dw_cuenta_embalaje"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_9 from statictext within w_mant_cambiaalturapacking
integer x = 1088
integer y = 564
integer width = 425
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Embalaje"
boolean focusrectangle = false
end type

type em_embalaje from editmask within w_mant_cambiaalturapacking
integer x = 1545
integer y = 548
integer width = 343
integer height = 92
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

event modified;Long  ll_numero
Integer li_planta, li_cliente
Date	ld_fecha

IF This.Text <> "" THEN
//	IF existemovimiento(Long(This.Text)) THEN
//		This.Text	=	""
//		
//		This.SetFocus()
//	ELSE
//		istr_mant.argumento[3]	=	String(Long(This.Text), '00000000')
//		ll_numero 	= Long(istr_mant.argumento[3])
//		li_cliente	= Integer(istr_mant.argumento[1])
//		li_planta	= Integer(istr_mant.argumento[2])
//		
//		SELECT Max(repe_tipopa)
//		INTO	:ii_tiporepa
//		FROM dba.repalletenca
//		WHERE clie_codigo = :li_cliente
//		AND	plde_codigo = :li_planta
//		AND	repe_nrosag = :ll_numero;
//		
//		SELECT Max(repe_fecrep)
//		INTO	:ld_fecha
//		FROM dba.repalletenca
//		WHERE clie_codigo = :li_cliente
//		AND	plde_codigo = :li_planta
//		AND	repe_nrosag = :ll_numero;
//		
//		pb_grabar.Enabled = True
//		
//	END IF
END IF
end event

type em_altura from editmask within w_mant_cambiaalturapacking
integer x = 626
integer y = 672
integer width = 343
integer height = 92
integer taborder = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

event modified;Long  ll_numero
Integer li_planta, li_cliente
Date	ld_fecha

//IF This.Text <> "" THEN
//	IF existemovimiento(Long(This.Text)) THEN
//		This.Text	=	""
//		
//		This.SetFocus()
//	ELSE
//		istr_mant.argumento[3]	=	String(Long(This.Text), '00000000')
//		ll_numero 	= Long(istr_mant.argumento[3])
//		li_cliente	= Integer(istr_mant.argumento[1])
//		li_planta	= Integer(istr_mant.argumento[2])
//		
//		SELECT Max(repe_tipopa)
//		INTO	:ii_tiporepa
//		FROM dba.repalletenca
//		WHERE clie_codigo = :li_cliente
//		AND	plde_codigo = :li_planta
//		AND	repe_nrosag = :ll_numero;
//		
//		SELECT Max(repe_fecrep)
//		INTO	:ld_fecha
//		FROM dba.repalletenca
//		WHERE clie_codigo = :li_cliente
//		AND	plde_codigo = :li_planta
//		AND	repe_nrosag = :ll_numero;
//		
//		pb_grabar.Enabled = True
//		
//	END IF
//END IF
end event

type st_7 from statictext within w_mant_cambiaalturapacking
integer x = 187
integer y = 684
integer width = 411
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Altura Actual"
boolean focusrectangle = false
end type

type em_nueva from editmask within w_mant_cambiaalturapacking
integer x = 1545
integer y = 672
integer width = 343
integer height = 92
integer taborder = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

event modified;Long  ll_numero
Integer li_planta, li_cliente
Date	ld_fecha

IF This.Text <> "" THEN
	IF valida_altura(em_altura.Text,This.Text,Long(em_numero.Text)) THEN
		This.Text	=	""
		This.SetFocus()
		pb_grabar.Enabled = False
	ELSE
		pb_grabar.Enabled = True
	END IF	
END IF
end event

type st_2 from statictext within w_mant_cambiaalturapacking
integer x = 1088
integer y = 684
integer width = 425
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Nueva Altura"
boolean focusrectangle = false
end type

type em_numero from editmask within w_mant_cambiaalturapacking
integer x = 622
integer y = 548
integer width = 453
integer height = 92
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

event modified;Long  ll_numero
Integer li_planta, li_cliente
Date	ld_fecha

IF This.TExt <> '' THEN
	IF existepalletfrigorifico(Long(This.Text)) THEN
		This.Text = ''
		This.SetFocus()
	ELSE
		IF Buscapallet(Long(This.Text)) THEN
			This.Text = ''
			This.SetFocus()
		ELSE
			em_altura.Text 	= is_altura
			em_embalaje.Text 	= is_embalaje
		END IF	
	END IF
END IF	

end event

type st_numero from statictext within w_mant_cambiaalturapacking
integer x = 178
integer y = 560
integer width = 480
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Pallet"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_mant_cambiaalturapacking
integer x = 626
integer y = 348
integer width = 1079
integer height = 108
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;IF NoExistePlanta(Integer(data)) THEN
	This.SetItem(1, "plde_codigo", gi_codplanta)
	
	RETURN 1
ELSE
	istr_mant.argumento[2]	=	String(data)
END IF
end event

event dberror;RETURN 1
end event

type dw_cliente from datawindow within w_mant_cambiaalturapacking
integer x = 626
integer y = 232
integer width = 1266
integer height = 92
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;IF NoExisteCliente(Integer(data)) THEN
	This.SetItem(1, "clie_codigo", gi_codexport)
	
	RETURN 1
ELSE
	istr_mant.argumento[1]	=	String(Integer(data), '000')
	idwc_planta.Retrieve(1)
	istr_mant.argumento[2]	=	String(dw_planta.Object.plde_codigo[1])
	dw_planta.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))
END IF
end event

event dberror;RETURN 1
end event

type st_4 from statictext within w_mant_cambiaalturapacking
integer x = 178
integer y = 360
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_mant_cambiaalturapacking
integer x = 178
integer y = 244
integer width = 311
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_5 from statictext within w_mant_cambiaalturapacking
integer x = 78
integer y = 68
integer width = 1970
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Cambio Altura Palletizaje"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_1 from statictext within w_mant_cambiaalturapacking
integer x = 82
integer y = 168
integer width = 1970
integer height = 336
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_mant_cambiaalturapacking
integer x = 2117
integer y = 800
integer width = 233
integer height = 196
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 12\Imagenes\Botones\SalirEnab.png"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_mant_cambiaalturapacking
integer x = 2117
integer y = 504
integer width = 233
integer height = 196
integer taborder = 50
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
boolean default = true
string picturename = "\Desarrollo 12\Imagenes\Botones\GrabaEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\GrabaDisab.png"
alignment htextalign = left!
end type

event clicked;actualiza_pallet(Long(em_numero.Text))

dw_1.Reset()
end event

type gb_2 from groupbox within w_mant_cambiaalturapacking
boolean visible = false
integer x = 2103
integer y = 452
integer width = 274
integer height = 268
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type gb_1 from groupbox within w_mant_cambiaalturapacking
boolean visible = false
integer x = 2103
integer y = 748
integer width = 274
integer height = 268
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type st_8 from statictext within w_mant_cambiaalturapacking
integer x = 82
integer y = 504
integer width = 1970
integer height = 296
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type sle_mensa from singlelineedit within w_mant_cambiaalturapacking
integer x = 178
integer y = 872
integer width = 1787
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_6 from statictext within w_mant_cambiaalturapacking
integer x = 82
integer y = 804
integer width = 1970
integer height = 224
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type


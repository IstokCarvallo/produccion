$PBExportHeader$w_borra_ultimo_repa.srw
$PBExportComments$Genera archivo Plano SAG por Repalletizaciones.
forward
global type w_borra_ultimo_repa from window
end type
type dw_1 from datawindow within w_borra_ultimo_repa
end type
type dw_planta from datawindow within w_borra_ultimo_repa
end type
type dw_cliente from datawindow within w_borra_ultimo_repa
end type
type st_4 from statictext within w_borra_ultimo_repa
end type
type st_3 from statictext within w_borra_ultimo_repa
end type
type em_fecha from editmask within w_borra_ultimo_repa
end type
type em_numero from editmask within w_borra_ultimo_repa
end type
type st_5 from statictext within w_borra_ultimo_repa
end type
type st_2 from statictext within w_borra_ultimo_repa
end type
type st_1 from statictext within w_borra_ultimo_repa
end type
type pb_salir from picturebutton within w_borra_ultimo_repa
end type
type pb_grabar from picturebutton within w_borra_ultimo_repa
end type
type gb_2 from groupbox within w_borra_ultimo_repa
end type
type gb_1 from groupbox within w_borra_ultimo_repa
end type
end forward

global type w_borra_ultimo_repa from window
integer width = 2455
integer height = 840
boolean titlebar = true
string title = "BORRA REPALLETIZADO"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 12632256
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
event ue_validapassword ( )
dw_1 dw_1
dw_planta dw_planta
dw_cliente dw_cliente
st_4 st_4
st_3 st_3
em_fecha em_fecha
em_numero em_numero
st_5 st_5
st_2 st_2
st_1 st_1
pb_salir pb_salir
pb_grabar pb_grabar
gb_2 gb_2
gb_1 gb_1
end type
global w_borra_ultimo_repa w_borra_ultimo_repa

type variables
str_mant			istr_mant
str_busqueda	istr_busq
Boolean			ib_Anulacion
Integer			ii_PlantaSag, ii_CantTarjas, ii_CantTarjasCambio, &
               ii_CantInspec, ii_CantInspecCambio, &
               ii_CantParticipa,ii_CantParticipaCambio, ii_tiporepa, ii_CantParticipa_2
Date				id_FechaRepa, id_FechaAltu
Date				id_FechaAcceso
Time				it_HoraAcceso
Long				il_proceso


DataWindowChild	idwc_cliente, idwc_planta

DataStore ids_palletencab
DataStore ids_inspeccion
end variables

forward prototypes
public function boolean noexistecliente (integer ai_cliente)
public function boolean noexisteplanta (integer ai_planta)
end prototypes

event ue_listo;w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

event ue_validapassword();Str_mant					lstr_mant
lstr_mant.Argumento[1]	=	"Producción"
lstr_mant.Argumento[2]	=	gs_Password

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

public function boolean noexistecliente (integer ai_cliente);String	ls_Nombre

SELECT	clie_nombre
	INTO	:ls_Nombre
	FROM	dba.clientesprod
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

public function boolean noexisteplanta (integer ai_planta);String	ls_Nombre
Integer	li_cliente

li_cliente	=	Integer(istr_mant.argumento[1])

SELECT	plde_nombre, plde_codsag
	INTO	:ls_Nombre, :ii_PlantaSag
	FROM	dba.plantadesp
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

on w_borra_ultimo_repa.create
this.dw_1=create dw_1
this.dw_planta=create dw_planta
this.dw_cliente=create dw_cliente
this.st_4=create st_4
this.st_3=create st_3
this.em_fecha=create em_fecha
this.em_numero=create em_numero
this.st_5=create st_5
this.st_2=create st_2
this.st_1=create st_1
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.gb_2=create gb_2
this.gb_1=create gb_1
this.Control[]={this.dw_1,&
this.dw_planta,&
this.dw_cliente,&
this.st_4,&
this.st_3,&
this.em_fecha,&
this.em_numero,&
this.st_5,&
this.st_2,&
this.st_1,&
this.pb_salir,&
this.pb_grabar,&
this.gb_2,&
this.gb_1}
end on

on w_borra_ultimo_repa.destroy
destroy(this.dw_1)
destroy(this.dw_planta)
destroy(this.dw_cliente)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.em_fecha)
destroy(this.em_numero)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.gb_2)
destroy(this.gb_1)
end on

event open;Long 		ll_numero
Integer 	li_TipoRepa
x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

dw_1.SetTransObject(SQLCA)

DataWindowChild	ldwc_especie

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

istr_mant.argumento[3]	=	String(gi_CodExport)
istr_mant.argumento[4]	=	String(gi_CodPlanta)

SELECT	 max(repe_numero)
	INTO	 :ll_numero
	FROM	dba.REPALLETENCA
	WHERE	clie_codigo	=	:gi_CodExport
	AND	plde_codigo	=	:gi_CodPlanta;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Repalletizados")
END IF
	
SELECT	repe_fecrep, repe_tipopa
	INTO	:id_FechaRepa, :li_TipoRepa
	FROM	dba.REPALLETENCA
	WHERE	clie_codigo	=	:gi_CodExport
	AND	plde_codigo	=	:gi_CodPlanta
	AND 	repe_numero = :ll_numero
	Group By repe_fecrep, repe_tipopa;	

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Repalletizados")
END IF

em_fecha.Text 	= String(id_FechaRepa)
em_numero.Text = String(ll_numero)

PostEvent("ue_validapassword")	
	
GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

IF em_numero.Text <> '' THEN
	pb_grabar.Enabled = True
ELSE
	pb_grabar.Enabled = False
	MessageBox("Atención", "No Existe Repalletizaje para estas Caracteristicas.")
	Return 1
END IF


							


end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type dw_1 from datawindow within w_borra_ultimo_repa
boolean visible = false
integer x = 1573
integer y = 932
integer width = 686
integer height = 400
integer taborder = 40
string title = "none"
string dataobject = "dw_borra_repa"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_planta from datawindow within w_borra_ultimo_repa
integer x = 750
integer y = 384
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Long		ll_numero
Integer	li_planta, li_cliente, li_TipoRepa

IF NoExistePlanta(Integer(data)) THEN
	This.SetItem(1, "plde_codigo", gi_codplanta)
	
	RETURN 1
ELSE
	istr_mant.argumento[4]	=	String(data)
	li_cliente 	= dw_cliente.Object.clie_codigo[1]
	li_planta	= Integer(istr_mant.argumento[4])
	
	SELECT	 max(repe_numero)
	INTO	 :ll_numero
	FROM	dba.REPALLETENCA
	WHERE	clie_codigo	=	:li_cliente
	AND	plde_codigo	=	:li_planta;

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Repalletizados")
	END IF
		
	SELECT	repe_fecrep, repe_tipopa
		INTO	:id_FechaRepa, :li_TipoRepa
		FROM	dba.REPALLETENCA
		WHERE	clie_codigo	=	:gi_CodExport
		AND	plde_codigo	=	:gi_CodPlanta
		AND 	repe_numero = 	:ll_numero
		Group By repe_fecrep, repe_tipopa;	
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Repalletizados")
	END IF
	
	em_fecha.Text 	= String(id_FechaRepa)
	em_numero.Text = String(ll_numero)
	
	IF em_numero.Text <> '' THEN
		pb_grabar.Enabled = True
	ELSE
		pb_grabar.Enabled = False
		MessageBox("Atención", "No existe Repalletizaje para esta Planta.", Exclamation!, Ok!)
		em_fecha.Text = ''
	END IF
END IF
end event

event dberror;RETURN 1
end event

event itemerror;Return 1
end event

type dw_cliente from datawindow within w_borra_ultimo_repa
integer x = 750
integer y = 232
integer width = 1157
integer height = 92
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Long		ll_numero
Integer	li_planta, li_cliente, li_TipoRepa

IF NoExisteCliente(Integer(data)) THEN
	This.SetItem(1, "clie_codigo", gi_codexport)
	RETURN 1
ELSE
	istr_mant.argumento[3]	=	String(Integer(data), '000')
	idwc_planta.Retrieve(1)
	istr_mant.argumento[4]	=	String(dw_planta.Object.plde_codigo[1])
	dw_planta.SetItem(1,"plde_codigo", Integer(istr_mant.argumento[4]))
	
	li_cliente 	= Integer(istr_mant.argumento[3])
	li_planta	= Integer(istr_mant.argumento[4])
	
	SELECT	 max(repe_numero)
	INTO	 :ll_numero
	FROM	dba.REPALLETENCA
	WHERE	clie_codigo	=	:li_cliente
	AND	plde_codigo	=	:li_planta;

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Repalletizados")
	END IF
		
	SELECT	repe_fecrep, repe_tipopa
		INTO	:id_FechaRepa, :li_TipoRepa
		FROM	dba.REPALLETENCA
		WHERE	clie_codigo	=	:gi_CodExport
		AND	plde_codigo	=	:gi_CodPlanta
		AND 	repe_numero = 	:ll_numero
		Group By repe_fecrep, repe_tipopa;	
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Repalletizados")
	END IF
	
	em_fecha.Text 	= String(id_FechaRepa)
	em_numero.Text = String(ll_numero)
	
	IF em_numero.Text <> '' THEN
		pb_grabar.Enabled = True
	ELSE
		pb_grabar.Enabled = False
		MessageBox("Atención", "No existe Repalletizaje para este Cliente.", Exclamation!, Ok!)
		em_fecha.Text = ''
	END IF

END IF
end event

event dberror;RETURN 1
end event

event itemerror;Return 1
end event

type st_4 from statictext within w_borra_ultimo_repa
integer x = 178
integer y = 396
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_borra_ultimo_repa
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
long backcolor = 12632256
string text = "Cliente"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_borra_ultimo_repa
integer x = 1531
integer y = 532
integer width = 357
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
string displaydata = "@"
end type

type em_numero from editmask within w_borra_ultimo_repa
event getfocus pbm_ensetfocus
event modified pbm_enmodified
integer x = 750
integer y = 532
integer width = 343
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "########"
string displaydata = "$"
end type

type st_5 from statictext within w_borra_ultimo_repa
integer x = 78
integer y = 64
integer width = 1970
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Borra Repalletizado"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_2 from statictext within w_borra_ultimo_repa
integer x = 183
integer y = 544
integer width = 535
integer height = 80
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Nro.Repalletizado"
boolean focusrectangle = false
end type

type st_1 from statictext within w_borra_ultimo_repa
integer x = 73
integer y = 168
integer width = 1970
integer height = 544
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_borra_ultimo_repa
integer x = 2158
integer y = 524
integer width = 155
integer height = 132
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\desarrollo\bmp\exit.bmp"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_borra_ultimo_repa
integer x = 2158
integer y = 236
integer width = 155
integer height = 132
integer taborder = 50
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
boolean default = true
string picturename = "\desarrollo\bmp\disksave.bmp"
string disabledname = "\desarrollo\bmp\disksavd.bmp"
alignment htextalign = left!
end type

event clicked;Long 		ll_numero, ll_fila
Integer 	li_respuesta

ll_numero = Long(em_numero.Text)

ll_fila = dw_1.retrieve(ll_numero,Integer(istr_mant.argumento[4]),&
								Integer(istr_mant.argumento[3]))

IF ll_fila = -1 THEN
	MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
	Information!, RetryCancel!)
	Return 1
ELSE
	li_respuesta = dw_1.Object.resultado[1]
	IF li_respuesta = 1 THEN
		MessageBox("Error", "No se puede eliminar el Repalletizaje ya que existe un Pallet Despachado.")
		Return 1
	ELSEIF li_respuesta = 2 THEN
		MessageBox("Atención", "Proceso realizado Satisfactoriamente.")
		pb_grabar.Enabled = False
		em_numero.Text = ''
		em_fecha.Text = ''
	END IF	
END IF				





end event

type gb_2 from groupbox within w_borra_ultimo_repa
integer x = 2094
integer y = 148
integer width = 274
integer height = 268
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_1 from groupbox within w_borra_ultimo_repa
integer x = 2094
integer y = 444
integer width = 274
integer height = 268
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type


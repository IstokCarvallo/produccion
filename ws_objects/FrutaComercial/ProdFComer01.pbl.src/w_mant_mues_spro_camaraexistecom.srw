$PBExportHeader$w_mant_mues_spro_camaraexistecom.srw
forward
global type w_mant_mues_spro_camaraexistecom from w_mant_directo
end type
type dw_2 from datawindow within w_mant_mues_spro_camaraexistecom
end type
type dw_planta from uo_dddw_planta within w_mant_mues_spro_camaraexistecom
end type
type st_8 from statictext within w_mant_mues_spro_camaraexistecom
end type
type em_fmovto from editmask within w_mant_mues_spro_camaraexistecom
end type
type dw_camaraorig from datawindow within w_mant_mues_spro_camaraexistecom
end type
type st_9 from statictext within w_mant_mues_spro_camaraexistecom
end type
type st_planta from statictext within w_mant_mues_spro_camaraexistecom
end type
type st_7 from statictext within w_mant_mues_spro_camaraexistecom
end type
type st_1 from statictext within w_mant_mues_spro_camaraexistecom
end type
type dw_camaradest from datawindow within w_mant_mues_spro_camaraexistecom
end type
type cb_aplica from commandbutton within w_mant_mues_spro_camaraexistecom
end type
end forward

global type w_mant_mues_spro_camaraexistecom from w_mant_directo
integer width = 3278
string title = "Traslado de Fruta Comercial en Camara"
dw_2 dw_2
dw_planta dw_planta
st_8 st_8
em_fmovto em_fmovto
dw_camaraorig dw_camaraorig
st_9 st_9
st_planta st_planta
st_7 st_7
st_1 st_1
dw_camaradest dw_camaradest
cb_aplica cb_aplica
end type
global w_mant_mues_spro_camaraexistecom w_mant_mues_spro_camaraexistecom

type variables
uo_plantadesp			iuo_plantadesp
uo_camarasfrigo		iuo_camarasfrigo


DataWindowChild		idwc_planta, idwc_camaraorig, idwc_camaradest

Integer					ii_planta, ii_tipomovto
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db ()
public function long duplicado (string planta, string especie, string lote, string secuen)
end prototypes

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_camaraorig.Enabled	=	True
	dw_camaradest.Enabled	=	True
	em_fmovto.Enabled	=	True
	em_fmovto.Text		=	''
	dw_camaraorig.Object.cama_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_camaradest.Object.cama_codigo.BackGround.Color	=	RGB(255,255,255)
ELSE
	dw_camaraorig.Enabled	=	False
	dw_camaradest.Enabled	=	False
	em_fmovto.Enabled	=	False
	dw_camaraorig.Object.cama_codigo.BackGround.Color	=	rgb(166,180,210)
	dw_camaradest.Object.cama_codigo.BackGround.Color	=	rgb(166,180,210)
END IF
end subroutine

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_1.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_1.Update(True, False) = 1 OR dw_2.Update(True, False) = 1  THEN
	Commit;

	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
			
		dw_1.ResetUpdate()
		dw_2.ResetUpdate()
			
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno


end function

public function long duplicado (string planta, string especie, string lote, string secuen);Long		ll_Fila
String	ls_codigo

ll_fila	= dw_1.Find("lofc_pltcod =" + planta + " AND " + "lofc_espcod =" + especie + " AND " + &
							"lofc_lotefc =" + lote   + " AND " + "lfcd_secuen =" + secuen, 1, dw_1.RowCount())

RETURN ll_Fila



end function

on w_mant_mues_spro_camaraexistecom.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.dw_planta=create dw_planta
this.st_8=create st_8
this.em_fmovto=create em_fmovto
this.dw_camaraorig=create dw_camaraorig
this.st_9=create st_9
this.st_planta=create st_planta
this.st_7=create st_7
this.st_1=create st_1
this.dw_camaradest=create dw_camaradest
this.cb_aplica=create cb_aplica
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.dw_planta
this.Control[iCurrent+3]=this.st_8
this.Control[iCurrent+4]=this.em_fmovto
this.Control[iCurrent+5]=this.dw_camaraorig
this.Control[iCurrent+6]=this.st_9
this.Control[iCurrent+7]=this.st_planta
this.Control[iCurrent+8]=this.st_7
this.Control[iCurrent+9]=this.st_1
this.Control[iCurrent+10]=this.dw_camaradest
this.Control[iCurrent+11]=this.cb_aplica
end on

on w_mant_mues_spro_camaraexistecom.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.dw_planta)
destroy(this.st_8)
destroy(this.em_fmovto)
destroy(this.dw_camaraorig)
destroy(this.st_9)
destroy(this.st_planta)
destroy(this.st_7)
destroy(this.st_1)
destroy(this.dw_camaradest)
destroy(this.cb_aplica)
end on

event ue_recuperadatos();call super::ue_recuperadatos;Long		ll_Fila, Respuesta, ll_Filaori, ll_Fila3

DO
	ll_Fila	= dw_2.Retrieve(Integer(istr_Mant.Argumento[2]), &
									 Integer(istr_Mant.Argumento[3]))

	IF ll_Fila = -1 THEN
		Respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_Fila > 0 THEN

			ll_Filaori	= dw_1.Retrieve(Integer(istr_Mant.Argumento[2]), &
									 Integer(istr_Mant.Argumento[4]))

			IF ll_Filaori = -1 THEN
				Respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
												
			END IF
			
		HabilitaEncab(False)

		dw_1.SetRow(1)
		dw_1.SetFocus()

		pb_insertar.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
		pb_imprimir.Enabled	= True
		dw_2.SetColumn("tot_bultos")
	ELSE
		pb_insertar.Enabled	= True
		pb_insertar.SetFocus()
	END IF

LOOP WHILE Respuesta = 1

IF Respuesta = 2 THEN Close(This)
end event

event ue_nuevo();call super::ue_nuevo;dw_2.SetColumn("tot_bultos")
end event

event ue_imprimir();SetPointer(HourGlass!)


Integer	li_Planta, li_origen, li_destino
Long		Fila
Date		ld_FechaMovto, ld_Fecha

str_info	lstr_info

lstr_info.titulo	= "INFORME TRASLADO DE CAMARAS"
lstr_info.copias	= 1

OpenWithParm(vinf, lstr_info)

vinf.dw_1.DataObject = "dw_info_spro_movtocamaracom"
vinf.dw_1.SetTransObject(sqlca)

li_Planta		=	dw_planta.Object.plde_codigo[1]
li_origen		=	Integer(Istr_mant.Argumento[3])
li_destino		=	Integer(Istr_mant.Argumento[4])

Fila = vinf.dw_1.Retrieve(li_Planta,li_destino)

IF Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE	
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Sort()
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event ue_guardar();IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN Return

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event open;/*
Argumentos
*********************************************
istr_Mant.Argumento[1]	=> Tipo de Movimiento
istr_Mant.Argumento[2]	=> Código de Planta
istr_Mant.Argumento[3]	=> Código de Camara
istr_Mant.Argumento[4]	=> Fecha del Movto.
*********************************************
*/

x				= 0
y				= 0

This.Height	= 1993
im_menu	= m_principal

This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	False
This.Icon									=	Gstr_apl.Icono

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

buscar	= "Camara:Ncama_codigo,Banda:Ncaex_nroban,Posición:Ncaex_nropos,Piso:Ncaex_nropis"
ordenar	= "Camara:cama_codigo,Banda:caex_nroban,Posición:caex_nropos,Piso:caex_nropis"

istr_Mant.Argumento[1]	=	Message.StringParm
istr_Mant.Argumento[2]	=	String(gstr_ParamPlanta.CodigoPlanta)

iuo_plantadesp			=	Create uo_plantadesp
iuo_camarasfrigo		=	Create uo_camarasfrigo
//iuo_Lotes				=	Create uo_lotesfrutagranel

//Planta
dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)
IF idwc_planta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
ELSE
	dw_planta.SetTransObject(SqlCa)
	dw_planta.InsertRow(0)
END IF

//Camara Origen
dw_camaraorig.GetChild("cama_codigo", idwc_camaraorig)
idwc_camaraorig.SetTransObject(SqlCa)
IF idwc_camaraorig.Retrieve(0) = 0 THEN
	MessageBox("Atención","Falta Registrar Camaras")
ELSE
	dw_camaraorig.SetTransObject(SqlCa)
	dw_camaraorig.InsertRow(0)
END IF

//Camara Destino
dw_camaradest.GetChild("cama_codigo", idwc_camaradest)
idwc_camaradest.SetTransObject(SqlCa)
IF idwc_camaradest.Retrieve(0) = 0 THEN
	MessageBox("Atención","Falta Registrar Camaras")
ELSE
	dw_camaradest.SetTransObject(SqlCa)
	dw_camaradest.InsertRow(0)
END IF

dw_planta.Enabled	=	False
dw_planta.SetItem(1, "plde_codigo", gstr_ParamPlanta.CodigoPlanta)
dw_planta.Object.plde_codigo.BackGround.Color		=	rgb(166,180,210)

dw_2.SetTransObject(SqlCa)

em_fmovto.Text = String(Today())
em_fmovto.SetFocus()
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_spro_camaraexistecom
boolean visible = false
integer y = 32
integer width = 2656
integer height = 336
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_spro_camaraexistecom
integer x = 2921
integer y = 372
integer taborder = 90
end type

event pb_nuevo::clicked;call super::clicked;HabilitaEncab(True)

dw_camaraorig.SetFocus()

dw_2.Reset()
dw_1.Reset()
em_fmovto.Text = String(Today())
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_spro_camaraexistecom
integer x = 2921
integer y = 136
integer taborder = 50
boolean enabled = false
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_spro_camaraexistecom
boolean visible = false
integer x = 2921
integer y = 784
integer taborder = 80
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_spro_camaraexistecom
boolean visible = false
integer x = 2921
integer y = 548
integer taborder = 0
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_spro_camaraexistecom
integer x = 2921
integer taborder = 130
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_spro_camaraexistecom
integer x = 2912
integer y = 860
integer taborder = 120
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_spro_camaraexistecom
integer x = 2917
integer y = 696
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_spro_camaraexistecom
integer y = 436
integer width = 2656
integer height = 664
integer taborder = 0
boolean titlebar = true
string title = "Camara Destino"
string dataobject = "dw_mant_mues_spro_camaraexistecom_dest"
boolean hscrollbar = true
boolean livescroll = false
end type

type dw_2 from datawindow within w_mant_mues_spro_camaraexistecom
integer x = 78
integer y = 1124
integer width = 2656
integer height = 640
integer taborder = 60
boolean bringtotop = true
boolean titlebar = true
string title = "Camara Origen"
string dataobject = "dw_mant_mues_spro_camaraexistecom"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event clicked;String ls_tecla

IF Keydown(KeyShifT!) THEN
	ls_tecla = "Shift"
ELSEIF KeyDown(KeyControl!) THEN
	ls_tecla = "Control"
END IF

F_Selecciona(This,ls_tecla,row)

IF dw_2.GetSelectedRow(0) = 0 THEN
	pb_grabar.Enabled	=	False
ELSE
	pb_grabar.Enabled	=	True
END IF	
end event

event itemerror;RETURN 1
end event

event itemchanged;String ls_Columna,ls_Null

SetNull(ls_Null)
CHOOSE CASE dwo.name
		
	CASE "tot_bultos"
		IF Integer(Data) > 0 THEN			
			IF  dw_2.Object.caex_canbul[row] < Integer(Data)THEN
				MessageBox("Error", "Bultos A traspasar es Mayor a Bultos Existentes.",Exclamation! )			
				dw_2.SetItem(row,"tot_bultos",0)	
				RETURN 1
			END IF			
		ELSE	
			RETURN 1
		END IF	
			
	END CHOOSE		
		
end event

type dw_planta from uo_dddw_planta within w_mant_mues_spro_camaraexistecom
integer x = 475
integer y = 104
integer taborder = 10
boolean bringtotop = true
integer ii_tipoplanta = 1
end type

event itemchanged;call super::itemchanged;Integer	li_Null

SetNull(li_Null)

IF NOT iuo_PlantaDesp.Existe(Integer(data),True,SqlCa) THEN
	This.SetItem(1, "plde_codigo", li_Null )
	This.SetFocus()
	RETURN 1
ELSE
	ii_Planta	=	Integer(data)
END IF

end event

event itemerror;call super::itemerror;RETURN 1
end event

type st_8 from statictext within w_mant_mues_spro_camaraexistecom
integer x = 1390
integer y = 116
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Camara Orig."
boolean focusrectangle = false
end type

type em_fmovto from editmask within w_mant_mues_spro_camaraexistecom
integer x = 475
integer y = 220
integer width = 361
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_Mant.Argumento[5] = This.Text
end event

type dw_camaraorig from datawindow within w_mant_mues_spro_camaraexistecom
integer x = 1801
integer y = 104
integer width = 873
integer height = 92
integer taborder = 30
boolean bringtotop = true
string dataobject = "dddw_camarasfrigo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Null

SetNull(li_Null)

IF NOT iuo_CamarasFrigo.Existe(Integer(istr_Mant.Argumento[2]),Integer(data),True,SqlCa) THEN
	This.SetItem(1, "cama_codigo", li_Null )
	This.SetFocus()
	RETURN 1
ELSE
	istr_Mant.Argumento[3]	=	Data
END IF

end event

event itemerror;RETURN 1
end event

type st_9 from statictext within w_mant_mues_spro_camaraexistecom
integer x = 1390
integer y = 232
integer width = 398
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Camara Dest."
boolean focusrectangle = false
end type

type st_planta from statictext within w_mant_mues_spro_camaraexistecom
integer x = 123
integer y = 116
integer width = 329
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Planta"
boolean focusrectangle = false
end type

type st_7 from statictext within w_mant_mues_spro_camaraexistecom
integer x = 123
integer y = 232
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Fe. Movto."
boolean focusrectangle = false
end type

type st_1 from statictext within w_mant_mues_spro_camaraexistecom
integer x = 78
integer y = 32
integer width = 2656
integer height = 336
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_camaradest from datawindow within w_mant_mues_spro_camaraexistecom
integer x = 1801
integer y = 220
integer width = 873
integer height = 92
integer taborder = 40
boolean bringtotop = true
string dataobject = "dddw_camarasfrigo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Null

SetNull(li_Null)

IF NOT iuo_CamarasFrigo.Existe(Integer(istr_Mant.Argumento[2]),Integer(data),True,SqlCa) THEN
	This.SetItem(1, "cama_codigo", li_Null )
	This.SetFocus()
	RETURN 1
ELSE
	istr_Mant.Argumento[4]	=	Data
	pb_lectura.Enabled		=	True
END IF

end event

event itemerror;RETURN 1
end event

type cb_aplica from commandbutton within w_mant_mues_spro_camaraexistecom
integer x = 2871
integer y = 1128
integer width = 279
integer height = 100
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Aplica"
end type

event clicked;Long		ll_fila1, ll_fila2, ll_canbul, ll_tobul
Integer	li_causal, li_NumeroLote
String	ls_NumeroLote

FOR ll_fila2 = 1 TO dw_2.RowCount()
	IF dw_2.IsSelected(ll_fila2) AND dw_2.Object.tot_bultos[ll_fila2] > 0 THEN
		ll_fila1 = 	Duplicado(String(dw_2.Object.lofc_pltcod[ll_fila2]), &
									 String(dw_2.Object.lofc_espcod[ll_fila2]), &
									 String(dw_2.Object.lofc_lotefc[ll_fila2]), &
									 String(dw_2.Object.lfcd_secuen[ll_fila2]))
		IF ll_fila1 = 0 THEN
			ll_fila1 = dw_1.InsertRow(0)
			dw_1.Object.lofc_pltcod[ll_fila1] = dw_2.Object.lofc_pltcod[ll_fila2]
			dw_1.Object.lofc_espcod[ll_fila1] = gstr_ParamPlanta.CodigoEspecie
			dw_1.Object.lofc_lotefc[ll_fila1] = dw_2.Object.lofc_lotefc[ll_fila2]
			dw_1.Object.lfcd_secuen[ll_fila1] = dw_2.Object.lfcd_secuen[ll_fila2]
			dw_1.Object.plde_codigo[ll_fila1] = dw_2.Object.plde_codigo[ll_fila2]
			dw_1.Object.cama_codigo[ll_fila1] = Integer(Istr_Mant.Argumento[4])
			dw_1.Object.caex_canbul[ll_fila1] = Long(dw_2.Object.tot_bultos[ll_fila2])
			
			dw_1.Object.tot_bultos[ll_fila1]	= dw_2.Object.tot_bultos[ll_fila2]
			
			/*Rebaje de Cajas*/
			dw_2.Object.caex_canbul[ll_fila2] =	Long(dw_2.Object.caex_canbul[ll_fila2])-Long(dw_2.Object.tot_bultos[ll_fila2])
			dw_2.Object.tot_bultos[ll_fila2]	 =	0		
		ELSE
			ll_canbul	= Long(dw_1.Object.caex_canbul[ll_fila1])
			ll_tobul		= Long(dw_2.Object.tot_bultos[ll_fila2])
			dw_1.Object.tot_bultos[ll_fila1]	= ll_tobul
			dw_1.Object.caex_canbul[ll_fila1] =ll_canbul +ll_tobul
			dw_2.Object.caex_canbul[ll_fila2] =	Long(dw_2.Object.caex_canbul[ll_fila2])-Long(dw_2.Object.tot_bultos[ll_fila2])
			dw_2.Object.tot_bultos[ll_fila2]	 =	0
			
		END IF	
	END IF	
	
NEXT	

end event


$PBExportHeader$w_mant_embalajesalida.srw
forward
global type w_mant_embalajesalida from w_mant_directo
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_embalajesalida
end type
type st_1 from statictext within w_mant_embalajesalida
end type
type st_2 from statictext within w_mant_embalajesalida
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_embalajesalida
end type
type st_3 from statictext within w_mant_embalajesalida
end type
type st_4 from statictext within w_mant_embalajesalida
end type
type ddlb_tipo from dropdownlistbox within w_mant_embalajesalida
end type
type em_proceso from editmask within w_mant_embalajesalida
end type
end forward

global type w_mant_embalajesalida from w_mant_directo
integer width = 2043
integer height = 2160
uo_selcliente uo_selcliente
st_1 st_1
st_2 st_2
uo_selplanta uo_selplanta
st_3 st_3
st_4 st_4
ddlb_tipo ddlb_tipo
em_proceso em_proceso
end type
global w_mant_embalajesalida w_mant_embalajesalida

type variables
Integer				ii_tipro

uo_embalajesprod	iuo_emb
end variables

forward prototypes
public subroutine wf_bloqueacolumnas (boolean ab_bloquea)
public subroutine cargaanterior ()
public function boolean duplicado (integer as_row, string as_valor)
end prototypes

public subroutine wf_bloqueacolumnas (boolean ab_bloquea);//
end subroutine

public subroutine cargaanterior ();Integer	li_orden, li_fila
Boolean	lb_retorno	=	False

dw_1.Reset()

SELECT max(orpr_numero)
  INTO :li_orden
  FROM dbo.spro_embalajesalida
 WHERE clie_codigo = :uo_selcliente.codigo
   AND plde_codigo = :uo_selplanta.codigo
	AND orpr_tipdoc = :ii_tipro
 USING sqlca;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, This.Title)
	
ELSE
	dw_1.Retrieve(uo_selcliente.codigo, uo_selplanta.codigo, ii_tipro, li_orden)
	
	IF dw_1.RowCount() > 0 THEN
		lb_Retorno	=	True
	END IF
END IF


FOR li_fila = 1 TO 3
	IF NOT lb_retorno THEN
		dw_1.InsertRow(0)
		dw_1.Object.emsa_codabc[li_fila]	=	char(64 + li_fila)
		dw_1.Object.clie_codigo[li_fila] = 	uo_selcliente.codigo
		dw_1.Object.plde_codigo[li_fila] = 	uo_selplanta.codigo
		dw_1.Object.orpr_tipdoc[li_fila] = 	ii_tipro
	END IF
	
	dw_1.Object.orpr_numero[li_fila] 	= 	Integer(em_proceso.Text)
	dw_1.SetItemStatus(li_fila, 0, Primary!, NewModified!)
	
NEXT
end subroutine

public function boolean duplicado (integer as_row, string as_valor);Integer	li_find

li_find	=	dw_1.Find("emba_codigo = '" + as_valor + "'", 1, dw_1.RowCount())

IF li_find > 0 AND li_find <> as_row THEN
	MessageBox("Error", "El embalaje que intenta ingresar ya ha sid digitado, ingrese otro", StopSign!)
	Return True
ELSE
	Return False
END IF
end function

on w_mant_embalajesalida.create
int iCurrent
call super::create
this.uo_selcliente=create uo_selcliente
this.st_1=create st_1
this.st_2=create st_2
this.uo_selplanta=create uo_selplanta
this.st_3=create st_3
this.st_4=create st_4
this.ddlb_tipo=create ddlb_tipo
this.em_proceso=create em_proceso
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_selcliente
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.uo_selplanta
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.st_4
this.Control[iCurrent+7]=this.ddlb_tipo
this.Control[iCurrent+8]=this.em_proceso
end on

on w_mant_embalajesalida.destroy
call super::destroy
destroy(this.uo_selcliente)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.uo_selplanta)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.ddlb_tipo)
destroy(this.em_proceso)
end on

event open;Boolean	lb_cerrar
x				= 0
y				= 0
This.Width	= st_encabe.width + 540
This.Height	= 1993
im_menu		= m_principal
iuo_emb		= Create uo_embalajesprod

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

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
								
								
IF IsNull(uo_SelCliente.Codigo) 		THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo) 		THEN lb_Cerrar	=	True

IF NOT lb_cerrar THEN
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	
	uo_SelCliente.Codigo	=	gi_CodExport
	uo_SelPlanta.Codigo	=	gi_CodPlanta
	
	uo_SelCliente.dw_seleccion.Object.Codigo[1]	=		gi_CodExport
	uo_SelPlanta.dw_seleccion.Object.Codigo[1]	=		gi_CodPlanta
	
	ddlb_tipo.SelectItem(1)
	ii_tipro					=	4
	
ELSE
	Close(THIS)
	
END IF
end event

event ue_recuperadatos;call super::ue_recuperadatos;Integer	li_proceso, li_filas, li_respuesta

w_main.SetMicroHelp("Recuperando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")


li_proceso	=	Integer(em_proceso.Text)

DO
	li_filas	=	dw_1.Retrieve(uo_selcliente.codigo, uo_selplanta.codigo, ii_tipro, li_proceso)

	IF li_filas = -1 THEN
		li_respuesta = MessageBox("Error", "Se ha producido un error en la base de datos, ¿Reintentar?", Exclamation!, YesNoCancel!, 1)
		IF li_respuesta = 2 THEN
			Close(This)
		END IF
	ELSE
		IF li_filas = 0 THEN
			CargaAnterior()
		END IF
		
		pb_imprimir.Enabled	=	True
		pb_eliminar.Enabled	=	True
		pb_insertar.Enabled	=	True
		pb_grabar.Enabled		=	True
		
	END IF
LOOP WHILE li_respuesta = 1
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_filas

//FOR li_filas = 1 TO dw_1.RowCount()
//	IF dw_1.Object.orpr_numero[li_filas] <> Integer(em_proceso.Text) THEN
//		dw_1.Object.orpr_numero[li_filas] 	= 	Integer(em_proceso.Text)
//		dw_1.SetItemStatus(li_filas, 0, Primary!, NewModified!)
//	END IF
//NEXT
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_embalajesalida
integer width = 1454
integer height = 556
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_embalajesalida
integer x = 1650
integer y = 720
integer taborder = 80
end type

type pb_lectura from w_mant_directo`pb_lectura within w_mant_embalajesalida
integer x = 1650
integer y = 116
integer taborder = 70
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_embalajesalida
integer x = 1650
integer y = 1080
integer taborder = 100
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_embalajesalida
integer x = 1650
integer y = 900
integer taborder = 90
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_embalajesalida
integer x = 1650
integer y = 1816
integer taborder = 130
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_embalajesalida
boolean visible = false
integer x = 1650
integer y = 1440
integer taborder = 120
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_embalajesalida
integer x = 1650
integer y = 1260
integer taborder = 110
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_embalajesalida
integer x = 274
integer y = 668
integer width = 1029
integer height = 1332
integer taborder = 60
boolean titlebar = true
string title = "Embalajes Genéricos"
string dataobject = "dw_mant_mues_embalajesalida"
boolean hscrollbar = true
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_nulo

SetNull(ls_nulo)

ls_columna	=	dwo.Name

CHOOSE CASE ls_columna
	CASE "emba_codigo"
		IF Duplicado(Row, Data) THEN
			This.Object.emba_codigo[Row]	=	ls_nulo
			Return 1
			
		ELSEIF NOT iuo_emb.Existe(uo_selcliente.codigo, data, True, sqlca) THEN
			This.Object.emba_codigo[Row]	=	ls_nulo
			Return 1
			
		END IF
		
END CHOOSE
end event

type uo_selcliente from uo_seleccion_clientesprod within w_mant_embalajesalida
integer x = 544
integer y = 136
integer height = 84
integer taborder = 10
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_1 from statictext within w_mant_embalajesalida
integer x = 128
integer y = 148
integer width = 288
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_embalajesalida
integer x = 128
integer y = 256
integer width = 288
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type uo_selplanta from uo_seleccion_plantas within w_mant_embalajesalida
integer x = 544
integer y = 232
integer height = 84
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_3 from statictext within w_mant_embalajesalida
integer x = 128
integer y = 472
integer width = 288
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Proceso"
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_embalajesalida
integer x = 128
integer y = 364
integer width = 393
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tipo Proceso"
boolean focusrectangle = false
end type

type ddlb_tipo from dropdownlistbox within w_mant_embalajesalida
integer x = 544
integer y = 340
integer width = 896
integer height = 448
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 16777215
boolean sorted = false
string item[] = {"Proceso","ReProceso","PreProceso","ReEmbalaje"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;CHOOSE CASE index
	CASE 1
		ii_tipro	=	4
		
	CASE 2
		ii_tipro	=	5
		
	CASE 3
		ii_tipro	=	8
		
	CASE 4
		ii_tipro	=	7
		
END CHOOSE
end event

type em_proceso from editmask within w_mant_embalajesalida
integer x = 549
integer y = 464
integer width = 402
integer height = 96
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
string mask = "########"
end type


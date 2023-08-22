$PBExportHeader$w_mant_mues_correlsellos.srw
forward
global type w_mant_mues_correlsellos from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_correlsellos
end type
type dw_plantadesp from datawindow within w_mant_mues_correlsellos
end type
end forward

global type w_mant_mues_correlsellos from w_mant_tabla
integer width = 3232
integer height = 2108
string title = "CORRELATIVOS SELLOS"
st_1 st_1
dw_plantadesp dw_plantadesp
end type
global w_mant_mues_correlsellos w_mant_mues_correlsellos

type variables
w_mant_deta_correlsellos  iw_mantencion

DataWindowchild dwc_plantas


end variables

event ue_imprimir;/* Este código no se hereda, debe ser sobreescrito en ventana descendiente */
SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "CORRELATIVOS SELLOS"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_correlsellos"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

on w_mant_mues_correlsellos.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_plantadesp=create dw_plantadesp
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_plantadesp
end on

on w_mant_mues_correlsellos.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_plantadesp)
end on

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False


OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF
	
	IF dw_1.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
		dw_1.SelectRow(il_fila,True)
	END IF
END IF

istr_mant.borra	 = False
end event

event open;Long ll_fila

x				= 0
y				= 0
This.Width	= dw_1.width + 540
This.Height	= 2500
im_menu	= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw	= dw_1

buscar	= "Código:Ncate_codigo,Descripción:Scate_nombre"
ordenar	= "Código:cate_codigo,Descripción:cate_nombre"

dw_plantadesp.GetChild("plde_codigo",dwc_plantas)
dwc_plantas.SetTransObject(Sqlca)
ll_fila = dwc_plantas.Retrieve(1)			//Plantas de Despacho
dw_plantadesp.SetTransObject(Sqlca)
dw_plantadesp.InsertRow(0)

IF ll_fila > 0 THEN dw_plantadesp.SetItem(1,"plde_codigo",gi_codplanta)
	
istr_mant.argumento[1]	=	String(gi_codplanta)						//Planta de Despacho
istr_mant.argumento[2]	= 	'1'	

//IF gstr_apl.CodigoSistema <> 23 THEN
//	IF gi_codexport = gi_cliebase THEN
//		IF gi_codplanta <> 2 AND gi_codplanta <> 3 AND gi_codplanta <> 4 THEN
//			istr_mant.Solo_Consulta = True
//		END IF	
//	END IF	
//END IF	


end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta
DO
	ll_fila	= dw_1.Retrieve(Integer(istr_mant.argumento[1]))
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_modifica;call super::ue_modifica;
IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False
	istr_mant.argumento[1]=String(dw_1.Object.plde_codigo[il_fila])

	OpenWithParm(iw_mantencion, istr_mant)
END IF

end event

event ue_nuevo;call super::ue_nuevo;
	istr_mant.borra	= False
	istr_mant.agrega	= True
		
	OpenWithParm(iw_mantencion, istr_mant)
	
	IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
		//pb_eliminar.Enabled	= TRUE
		pb_grabar.Enabled		= TRUE
	END IF
	
	dw_1.SetRow(il_fila)
	dw_1.SelectRow(il_fila,True)

end event

event resize;//
end event

event ue_listo;IF dw_1.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
	
	IF istr_mant.Solo_Consulta THEN
		pb_Eliminar.Enabled	=	False
		pb_Grabar.Enabled		=	False
		pb_Insertar.Enabled	=	False
	ELSE
		//pb_Eliminar.Enabled	=	True
		pb_Grabar.Enabled		=	True
		pb_Insertar.Enabled	=	True
	END IF
ELSE
	IF istr_mant.Solo_Consulta THEN
		pb_Insertar.Enabled	=	False
	ELSE
		pb_Insertar.Enabled	=	True
	END IF
END IF

w_main.SetMicroHelp("Listo")

SetPointer(Arrow!)
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_filon, ll_ultimo, ll_conta
Integer	li_Planta

ll_ultimo	=	dw_1.RowCount()

li_Planta	=	Integer(istr_mant.argumento[1])

IF ll_ultimo > 0 THEN

	dw_1.Object.sell_vigenc[ll_ultimo]	=	0	
	
	select max(sell_secuen) into :ll_conta
	 from dba.correlsellos
	 where plde_codigo = :li_planta;
	 
	 IF IsNull(ll_conta) THEN
		ll_conta = 0
	 END IF
END IF

FOR ll_filon	=	1 TO ll_ultimo
    IF IsNull(dw_1.Object.sell_secuen[ll_filon]) THEN
		 ll_conta++
		 dw_1.Object.sell_secuen[ll_filon] = ll_conta
	END IF

NEXT

IF ll_ultimo > 1 THEN
	
	FOR ll_filon	=	1 TO (ll_ultimo - 1)
		
		 dw_1.Object.sell_vigenc[ll_filon]	=	1
			 
	NEXT

END IF

end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_correlsellos
integer x = 69
integer y = 276
integer width = 2478
integer height = 1320
string dataobject = "dw_mues_correlsellos"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_correlsellos
integer x = 69
integer y = 40
integer width = 2478
integer height = 208
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_correlsellos
integer x = 2651
integer y = 92
integer taborder = 20
end type

event pb_lectura::clicked;call super::clicked;dw_plantadesp.Enabled	=	FALSE
dw_plantadesp.Modify("plde_codigo.background.color = " + string(RGB(166,180,210)))




end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_correlsellos
integer x = 2651
integer y = 408
integer taborder = 40
end type

event pb_nuevo::clicked;call super::clicked;dw_plantadesp.Enabled	=	True
dw_plantadesp.Modify("plde_codigo.background.color = " + string(rgb(255,255,255)))



end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_correlsellos
integer x = 2651
integer y = 652
integer taborder = 50
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_correlsellos
integer x = 2651
integer y = 896
integer taborder = 60
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_correlsellos
integer x = 2651
integer y = 1144
integer taborder = 70
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_correlsellos
integer x = 2651
integer y = 1388
integer taborder = 80
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_correlsellos
integer x = 2651
integer y = 1676
integer taborder = 90
end type

type st_1 from statictext within w_mant_mues_correlsellos
integer x = 347
integer y = 116
integer width = 219
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type dw_plantadesp from datawindow within w_mant_mues_correlsellos
integer x = 626
integer y = 108
integer width = 969
integer height = 92
integer taborder = 60
boolean bringtotop = true
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String		ls_Columna[]

IF ExistePlanta(Integer(istr_mant.Argumento[1]), Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1] = data
ELSE
	RETURN 1
END IF
end event


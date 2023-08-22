﻿$PBExportHeader$w_mant_admausuaropcio_new.srw
$PBExportComments$Mantención de Accesos por Grupo y/o Usuario.
forward
global type w_mant_admausuaropcio_new from w_mant_directo
end type
type gb_4 from groupbox within w_mant_admausuaropcio_new
end type
type tv_1 from treeview within w_mant_admausuaropcio_new
end type
type st_1 from statictext within w_mant_admausuaropcio_new
end type
type st_2 from statictext within w_mant_admausuaropcio_new
end type
type rb_grupos from radiobutton within w_mant_admausuaropcio_new
end type
type rb_usuarios from radiobutton within w_mant_admausuaropcio_new
end type
type dw_usuario from datawindow within w_mant_admausuaropcio_new
end type
type st_3 from statictext within w_mant_admausuaropcio_new
end type
type dw_grupo from datawindow within w_mant_admausuaropcio_new
end type
end forward

global type w_mant_admausuaropcio_new from w_mant_directo
integer width = 3598
integer height = 2028
string title = "MANTENCION DE ACCESO AL SISTEMA"
event ue_pueblatreeview ( )
gb_4 gb_4
tv_1 tv_1
st_1 st_1
st_2 st_2
rb_grupos rb_grupos
rb_usuarios rb_usuarios
dw_usuario dw_usuario
st_3 st_3
dw_grupo dw_grupo
end type
global w_mant_admausuaropcio_new w_mant_admausuaropcio_new

type variables
TreeViewItem		itvi_Item
DataWindowChild	idwc_usuarios, idwc_grupos

String	is_GrupoNombre, is_Usuario, is_UsuarioNombre
Integer	ii_Grupo
end variables

forward prototypes
public subroutine buscausuario ()
public function string etiqueta (string as_texto)
public function integer of_add_items (long al_parent, integer ai_level, integer ai_rows)
public subroutine of_set_item (integer ai_level, integer ai_row, treeview atvi_new)
public subroutine validaacceso ()
end prototypes

event ue_pueblatreeview();Integer	li_Nivel1, li_Nivel2, li_Nivel3, li_Nivel4, li_Nivel5
Long		ll_Padre1, ll_Padre2, ll_Padre3, ll_Padre4, ll_Padre5

im_menu = m_principal

SetPointer(HourGlass!)
	
FOR li_Nivel1 = 2 TO UpperBound(im_menu.Item[])
	IF im_menu.Item[li_Nivel1].Visible THEN
		itvi_Item.Data							=	im_menu.Item[li_Nivel1].ClassName()
		itvi_Item.Label						=	Etiqueta(im_menu.Item[li_Nivel1].Text)
		itvi_Item.PictureIndex				=	1
		itvi_Item.SelectedPictureIndex	=	2
			
		IF Pos("m_window%%m_help%%m_topicactions", itvi_Item.Data) = 0 THEN
			IF UpperBound(im_menu.Item[li_Nivel1].Item[]) > 0  THEN
				itvi_Item.Children		=	True
				ll_Padre1					=	tv_1.InsertItemLast(0, itvi_Item)
				
				FOR li_Nivel2 = 1 TO UpperBound(im_menu.Item[li_Nivel1].Item[])
					IF im_menu.Item[li_Nivel1].Item[li_Nivel2].Visible THEN
						itvi_Item.Data  						=	im_menu.Item[li_Nivel1].Item[li_Nivel2].ClassName()
						itvi_Item.Label						=	Etiqueta(im_menu.Item[li_Nivel1].Item[li_Nivel2].Text)
						itvi_Item.PictureIndex				=	1
						itvi_Item.SelectedPictureIndex	=	2
						
						IF UpperBound(im_menu.Item[li_Nivel1].Item[li_Nivel2].Item[]) > 0  THEN
							itvi_Item.Children		=	True
							ll_Padre2					=	tv_1.InsertItemLast(ll_Padre1, itvi_Item)
					
							FOR li_Nivel3 = 1 TO UpperBound(im_menu.Item[li_Nivel1].Item[li_Nivel2].Item[])
								IF im_menu.Item[li_Nivel1].Item[li_Nivel2].Item[li_Nivel3].Visible THEN
									itvi_Item.Data  						=	im_menu.Item[li_Nivel1].Item[li_Nivel2].Item[li_Nivel3].ClassName()
									itvi_Item.Label						=	Etiqueta(im_menu.Item[li_Nivel1].Item[li_Nivel2].Item[li_Nivel3].Text)
									itvi_Item.PictureIndex				=	1
									itvi_Item.SelectedPictureIndex	=	2
									
									IF UpperBound(im_menu.Item[li_Nivel1].Item[li_Nivel2].Item[li_Nivel3].Item[]) > 0  THEN		
										itvi_Item.Children		=	True
										ll_Padre3					=	tv_1.InsertItemLast(ll_Padre2, itvi_Item)
						
										FOR li_Nivel4 = 1 TO UpperBound(im_menu.Item[li_Nivel1].Item[li_Nivel2].Item[li_Nivel3].Item[])
											IF im_menu.Item[li_Nivel1].Item[li_Nivel2].Item[li_Nivel3].Item[li_Nivel4].Visible THEN
												itvi_Item.Data  						=	im_menu.Item[li_Nivel1].Item[li_Nivel2].Item[li_Nivel3].Item[li_Nivel4].ClassName()
												itvi_Item.Label						=	Etiqueta(im_menu.Item[li_Nivel1].Item[li_Nivel2].Item[li_Nivel3].Item[li_Nivel4].Text)
												itvi_Item.PictureIndex				=	1
												itvi_Item.SelectedPictureIndex	=	2
												
												IF UpperBound(im_menu.Item[li_Nivel1].Item[li_Nivel2].Item[li_Nivel3].Item[li_Nivel4].Item[]) > 0  THEN		
													itvi_Item.Children		=	True
													ll_Padre4					=	tv_1.InsertItemLast(ll_Padre3, itvi_Item)
							
													FOR li_Nivel5 = 1 TO UpperBound(im_menu.Item[li_Nivel1].Item[li_Nivel2].Item[li_Nivel3].Item[li_Nivel4].Item[li_Nivel5].Item[])
														itvi_Item.Data  						=	im_menu.Item[li_Nivel1].Item[li_Nivel2].Item[li_Nivel3].Item[li_Nivel4].Item[li_Nivel5].ClassName()
														itvi_Item.Label						=	Etiqueta(im_menu.Item[li_Nivel1].Item[li_Nivel2].Item[li_Nivel3].Item[li_Nivel4].Item[li_Nivel5].Text)
														itvi_Item.PictureIndex				=	1
														itvi_Item.SelectedPictureIndex	=	2
														itvi_Item.Children					=	False
														ll_Padre5								=	tv_1.InsertItemLast(ll_Padre4, itvi_Item)
													NEXT
												ELSE
													itvi_Item.Children		=	False
													ll_Padre4					=	tv_1.InsertItemLast(ll_Padre3, itvi_Item)
												END IF
											END IF
										NEXT
									ELSE
										itvi_Item.Children		=	False
										ll_Padre3					=	tv_1.InsertItemLast(ll_Padre2, itvi_Item)
									END IF
								END IF
							NEXT
						ELSE
							itvi_Item.Children		=	False
							ll_Padre2					=	tv_1.InsertItemLast(ll_Padre1, itvi_Item)
						END IF
					END IF
				NEXT
			ELSE
				itvi_Item.Children		=	False
				ll_Padre1					=	tv_1.InsertItemLast(0, itvi_Item)
			END IF
		END IF
	END IF
NEXT
end event

public subroutine buscausuario ();
end subroutine

public function string etiqueta (string as_texto);IF as_Texto = "-" THEN
	as_Texto	=	"__________________________"
ELSE
	as_Texto	=	F_Global_Replace(as_Texto, "&", "")
END IF

RETURN as_Texto
end function

public function integer of_add_items (long al_parent, integer ai_level, integer ai_rows);// Function to add the items to the TreeView using the data in the DataStore

Integer				li_Cnt
TreeViewItem		ltvi_New
TreeView          ltvi_Nuevo

// Add each item to the TreeView
For li_Cnt = 1 To ai_Rows
	// Call a function to set the values of the TreeView item from 
	// the DataStore data
	of_set_item(ai_Level, li_Cnt, ltvi_Nuevo)
	
	// Add the item after the last child
	If tv_1.InsertItemLast(al_Parent, ltvi_New) < 1 Then
		// Error
		MessageBox("Error", "Error inserting item", Exclamation!)
		Return -1
	End If
Next

Return ai_Rows

end function

public subroutine of_set_item (integer ai_level, integer ai_row, treeview atvi_new);//// Set the Lable and Data attributes for the new item from the data in the DataStore
//
//Integer	li_Picture
//
//Choose Case ai_Level
//	Case 1
//		atvi_New.Label = ids_Data[1].Object.grpo_codigo[ai_Row] + ", " + &
//							  ids_Data[1].Object.grpo_nombre[ai_Row]
//	Case 2
//		atvi_New.Label = ids_Data[2].Object.sist_codigo[ai_Row]
//		atvi_New.Data  = ids_Data[2].Object.usop_grpusu[ai_Row]
//		atvi_New.Data  = ids_Data[2].Object.usop_habili[ai_Row]
//		atvi_New.Data  = ids_Data[2].Object.usop_visibl[ai_Row]		
//		atvi_New.Data  = ids_Data[2].Object.grpo_codigo[ai_Row]		
//		atvi_New.Data  = ids_Data[2].Object.sist_codigo[ai_Row]		
//		atvi_New.Data  = ids_Data[2].Object.grpo_nombre[ai_Row]		
//End Choose
//
//
//If ai_Level < 2 Then
//	atvi_New.Children = True
//	atvi_New.PictureIndex = ai_Level
//	atvi_New.SelectedPictureIndex = 2
//Else
//	atvi_New.Children = False
//	
//	// Set the picture to be the product picture
//	//li_Picture = tv_1.AddPicture(ids_Data[4].Object.product_picture_name[ai_Row])
//	//atvi_New.PictureIndex = li_Picture
//	//atvi_New.SelectedPictureIndex = 4
//End If
//
end subroutine

public subroutine validaacceso ();Integer li_HabGrupo = 1, li_VisGrupo = 1
String  ls_CodMenu

ls_CodMenu	=	itvi_Item.Data

IF rb_Usuarios.checked THEN
	SELECT	usop_habili, usop_visibl 
		INTO	:li_HabGrupo, :li_VisGrupo
		FROM	dba.admausuaropcio
		WHERE	sist_codigo	=	:gstr_apl.CodigoSistema
		AND	grpo_codigo	=	:ii_Grupo
		AND	usua_codigo	=	'TODOS'
		AND	usop_codmen	=	:ls_CodMenu ;
	
	IF sqlca.sqlcode	=	-1	THEN
		F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla AdmaUsuarOpcio")
	ELSE
		IF li_VisGrupo = 0 THEN
			MessageBox("Error de Acceso", "El Grupo al que pertenece el Usuario " + &
			is_UsuarioNombre + "~rno tiene acceso al módulo, por lo tanto " + &
			"este usuario tampoco tiene acceso.")
			
			RETURN
		ELSEIF li_VisGrupo = 1 AND li_HabGrupo = 0 THEN
			TriggerEvent("ue_recuperadatos")
			
			dw_1.Object.usop_visibl.protect	=	1
			dw_1.Object.usop_habili.protect	=	1
		ELSEIF li_VisGrupo = 1 AND li_HabGrupo = 1 THEN
			TriggerEvent("ue_recuperadatos")
		END IF
	END IF
ELSE
	TriggerEvent("ue_recuperadatos")
END IF
end subroutine

on w_mant_admausuaropcio_new.create
int iCurrent
call super::create
this.gb_4=create gb_4
this.tv_1=create tv_1
this.st_1=create st_1
this.st_2=create st_2
this.rb_grupos=create rb_grupos
this.rb_usuarios=create rb_usuarios
this.dw_usuario=create dw_usuario
this.st_3=create st_3
this.dw_grupo=create dw_grupo
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_4
this.Control[iCurrent+2]=this.tv_1
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.rb_grupos
this.Control[iCurrent+6]=this.rb_usuarios
this.Control[iCurrent+7]=this.dw_usuario
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.dw_grupo
end on

on w_mant_admausuaropcio_new.destroy
call super::destroy
destroy(this.gb_4)
destroy(this.tv_1)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.rb_grupos)
destroy(this.rb_usuarios)
destroy(this.dw_usuario)
destroy(this.st_3)
destroy(this.dw_grupo)
end on

event open;//istr_mant.argumento[1]	=	Código de Grupo
//istr_mant.argumento[2]	=	Código de Usuario

x				=	0
y				=	0
im_menu		=	Create m_principal

dw_1.SetTransObject(sqlca)

dw_grupo.GetChild("grpo_codigo", idwc_grupos)
idwc_grupos.SetTransObject(sqlca)

IF idwc_grupos.Retrieve(gstr_apl.CodigoSistema) = 0 THEN
	MessageBox("Atención", "No Existen Grupos Asociados a este Sistema")
	
	Close(This)
ELSE
	dw_grupo.InsertRow(0)
	dw_usuario.InsertRow(0)
	dw_usuario.GetChild("usua_codigo", idwc_usuarios)
	idwc_usuarios.SetTransObject(sqlca)
	
	ii_Grupo						=	dw_grupo.Object.grpo_codigo[1]
	istr_mant.argumento[1]	=	String(ii_Grupo)
	istr_mant.argumento[2]	=	"TODOS"
	
	dw_1.InsertRow(0)
	
	rb_grupos.TriggerEvent(Clicked!)
	
	PostEvent("ue_pueblatreeview")
	
	GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
								This.Title, "Acceso a Aplicación", 1)
END IF
end event

event resize;Integer		li_posi_y, li_objeto

//dw_1.Resize(This.WorkSpaceWidth() - 510,This.WorkSpaceHeight() - dw_1.y - 75)

//dw_1.x					= 78
//st_encabe.width		= dw_1.width

gb_1.x 					= This.WorkSpaceWidth() - 351
gb_1.y 					= 33
gb_1.width				= 275
gb_1.height				= 180 * 1 + 97 /*  (1 Botón)  */

pb_lectura.x			= This.WorkSpaceWidth() - 292
pb_lectura.y			= gb_1.y + 88
pb_lectura.width		= 156
pb_lectura.height		= 133

gb_2.x 					= This.WorkSpaceWidth() - 351
gb_2.width				= 275

pb_nuevo.x				= This.WorkSpaceWidth() - 292
pb_nuevo.width			= 156
pb_nuevo.height		= 133

pb_insertar.x			= This.WorkSpaceWidth() - 292
pb_insertar.width		= 156
pb_insertar.height	= 133

pb_eliminar.x			= This.WorkSpaceWidth() - 292
pb_eliminar.width		= 156
pb_eliminar.height	= 133

pb_grabar.x				= This.WorkSpaceWidth() - 292
pb_grabar.width		= 156
pb_grabar.height		= 133

pb_imprimir.x			= This.WorkSpaceWidth() - 292
pb_imprimir.width		= 156
pb_imprimir.height	= 133

IF st_encabe.Visible THEN
	gb_2.y 					= dw_1.y - 36
ELSE
	gb_2.y 					= gb_1.y + gb_1.height + 23
END IF

li_posi_y	= gb_2.y - 92

IF pb_nuevo.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_nuevo.y	= li_posi_y
END IF

IF pb_insertar.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_insertar.y	= li_posi_y
END IF

IF pb_eliminar.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_eliminar.y	= li_posi_y
END IF

IF pb_grabar.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_grabar.y		= li_posi_y
END IF

IF pb_imprimir.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_imprimir.y	= li_posi_y
END IF

gb_2.height				= 180 * li_objeto + 97
gb_3.x 					= This.WorkSpaceWidth() - 351
gb_3.y 					= This.WorkSpaceHeight() - 345
gb_3.width				= 275
gb_3.height				= 180 * 1 + 97 /*  (1 Botón)  */

pb_salir.x				= This.WorkSpaceWidth() - 292
pb_salir.y				= gb_3.y + 88
pb_salir.width			= 156
pb_salir.height		= 133
end event

event ue_recuperadatos;
Integer li_HabGrupo, li_ViGrupo, li_Codigo
String ls_CodMenu


dw_1.SetRedraw(False)

dw_1.Retrieve(gstr_apl.CodigoSistema, Integer(istr_mant.argumento[1]), &
				 istr_mant.argumento[2], itvi_item.Data)
					
IF dw_1.RowCount()=0 THEN
	dw_1.InsertRow(0)
	
	dw_1.SetItem(1, "sist_nombre", gstr_apl.NombreSistema)
	dw_1.SetItem(1, "usop_nommen", itvi_item.Label)			
	dw_1.SetITem(1, "grpo_nombre", is_GrupoNombre)
	dw_1.SetItem(1, "usua_codigo", istr_mant.argumento[2])
	dw_1.SetItem(1, "sist_codigo", gstr_apl.CodigoSistema)					
	dw_1.SetItem(1, "grpo_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(1, "usop_codmen", itvi_item.data)
	
	pb_imprimir.Enabled	=	False
	pb_grabar.Enabled		=	True
ELSE
	pb_imprimir.Enabled	=	True
	pb_grabar.Enabled		=	True
END IF

dw_1.SetRedraw(True)
end event

event ue_borrar;call super::ue_borrar;PostEvent("ue_nuevo")
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

//lstr_info.titulo	= "TITULO DEL INFORME"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_admausuaropcio"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(gstr_apl.codigosistema, gstr_apl.nombresistema, Integer(istr_mant.argumento[1]), istr_mant.argumento[2])

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event ue_nuevo();call super::ue_nuevo;pb_grabar.Enabled		=	False
pb_eliminar.Enabled	=	False
dw_grupo.TriggerEvent(clicked!)
dw_grupo.Reset()
dw_grupo.InsertRow(0)
dw_usuario.Reset()
dw_usuario.InsertRow(0)
istr_mant.argumento[1]	=	""
istr_mant.argumento[2]	=	""
	

end event

event ue_antesguardar();call super::ue_antesguardar;//Integer li_visible, li_habile, li_soloco
//
//li_visible	=	dw_1.Object.usop_visibl[1] 
//li_habile	=	dw_1.Object.usop_habili[1]
//li_soloco	=	dw_1.Object.usop_soloco[1] 
//		
//IF dw_1.RowCount() > 0 THEN
//	IF dw_1.Object.usop_visibl[1] = 1 AND &
//		dw_1.Object.usop_habili[1] = 1 AND &
//		dw_1.Object.usop_soloco[1] = 0 THEN
//		dw_1.DeleteRow(1)
//	END IF
//END IF
//		
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event mousemove;//
end event

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_admausuaropcio_new
integer x = 3250
integer y = 456
integer taborder = 50
end type

event pb_nuevo::clicked;call super::clicked;CHOOSE CASE wf_modifica()
	CASE 0
		CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
			CASE 1
				Message.DoubleParm = 0
				Parent.TriggerEvent("ue_guardar")
				IF message.DoubleParm = -1 THEN RETURN
				
			CASE 3
				RETURN
		END CHOOSE
		
END CHOOSE

Parent.PostEvent("ue_nuevo")
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_admausuaropcio_new
boolean visible = false
integer x = 3301
integer y = 176
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_admausuaropcio_new
boolean visible = false
integer x = 3250
integer y = 600
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_admausuaropcio_new
boolean visible = false
integer x = 3305
integer y = 600
integer taborder = 80
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_admausuaropcio_new
integer x = 3250
integer y = 1196
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_admausuaropcio_new
integer x = 3250
integer y = 912
integer taborder = 110
boolean enabled = true
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_admausuaropcio_new
integer x = 3250
integer y = 752
integer taborder = 100
end type

event pb_grabar::clicked;Parent.TriggerEvent("ue_guardar")
end event

type dw_1 from w_mant_directo`dw_1 within w_mant_admausuaropcio_new
integer x = 1339
integer y = 860
integer width = 1778
integer height = 988
integer taborder = 70
string dataobject = "dw_mant_admausuariopcio"
boolean hscrollbar = true
boolean vscrollbar = false
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna

ls_columna = dwo.Name

CHOOSE CASE ls_columna
	CASE "usop_visibl", "usop_habili"
		This.SetItem(il_fila, "usop_soloco", 0)

END CHOOSE
end event

type gb_1 from w_mant_directo`gb_1 within w_mant_admausuaropcio_new
boolean visible = false
integer x = 3241
integer y = 88
end type

type gb_3 from w_mant_directo`gb_3 within w_mant_admausuaropcio_new
integer x = 3191
integer y = 1108
end type

type gb_2 from w_mant_directo`gb_2 within w_mant_admausuaropcio_new
integer x = 3191
integer y = 372
integer height = 724
end type

type st_encabe from w_mant_directo`st_encabe within w_mant_admausuaropcio_new
boolean visible = false
integer x = 1335
integer y = 56
integer width = 1783
integer height = 560
end type

type gb_4 from groupbox within w_mant_admausuaropcio_new
integer x = 1403
integer y = 88
integer width = 1655
integer height = 188
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type tv_1 from treeview within w_mant_admausuaropcio_new
event type long ue_selectionchanged ( any oldhandle,  long newhandle )
integer x = 64
integer y = 44
integer width = 1198
integer height = 1804
integer taborder = 60
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
boolean linesatroot = true
string picturename[] = {"Custom027!","Custom026!",""}
long picturemaskcolor = 553648127
long statepicturemaskcolor = 536870912
end type

event selectionchanged;This.GetItem(NewHandle, itvi_Item)

ValidaAcceso()
end event

event selectionchanging;Integer	li_Grupo

li_Grupo	=	dw_Grupo.Object.grpo_codigo[1]

IF	This.GetItem(NewHandle, itvi_Item) > 0 AND IsNull(li_Grupo) THEN
	MessageBox("Atención", "Primero debe seleccionar por lo menos el grupo")
	tv_1.SetReDraw(True)
ELSE
	ValidaAcceso()	
END IF
end event

type st_1 from statictext within w_mant_admausuaropcio_new
integer x = 1577
integer y = 348
integer width = 192
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Grupo"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_admausuaropcio_new
integer x = 1577
integer y = 452
integer width = 219
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Usuario"
boolean focusrectangle = false
end type

type rb_grupos from radiobutton within w_mant_admausuaropcio_new
integer x = 1682
integer y = 160
integer width = 293
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Grupos"
boolean checked = true
end type

event clicked;dw_grupo.Enabled	=	True
dw_grupo.Reset()
dw_grupo.InsertRow(0)	
dw_grupo.SetFocus()	
idwc_grupos.Retrieve(gstr_apl.CodigoSistema)

dw_usuario.Enabled										=	False
istr_mant.Argumento[2]									=	"TODOS"
dw_usuario.Object.usua_codigo.BackGround.Color	=	RGB(192, 192, 192)	
dw_usuario.Reset()
dw_usuario.InsertRow(0)

pb_grabar.Enabled	=	False
end event

type rb_usuarios from radiobutton within w_mant_admausuaropcio_new
integer x = 2441
integer y = 160
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Usuarios"
end type

event clicked;Integer li_Existe

SELECT	Count(*)
	INTO	:li_Existe
	FROM	dba.admagrupousuario
	WHERE	grpo_codigo	=	:ii_Grupo
	AND	sist_codigo = 	:gstr_apl.codigosistema;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Admausuarios al~r "+&
								  "elegir opción Usuarios")
ELSEIF li_Existe = 0 THEN
	MessageBox("Atención", "El Grupo no tiene Usuarios que mostrar")
ELSE
	dw_usuario.Enabled										=	True
	dw_usuario.Object.usua_codigo.BackGround.Color	=	RGB(255, 255, 255)	
	dw_usuario.Getchild("usua_codigo", idwc_usuarios)
//	idwc_usuarios.Retrieve()
	idwc_usuarios.Retrieve(ii_Grupo,gstr_apl.codigosistema)
END IF
end event

type dw_usuario from datawindow within w_mant_admausuaropcio_new
integer x = 1833
integer y = 452
integer width = 974
integer height = 92
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_admausuarios"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Nula
Integer	li_Grupo

SetNull(ls_Nula)

SELECT	usua_nombre, grpo_codigo
	INTO	:is_UsuarioNombre, :li_Grupo
	FROM	dba.admausuarios as usua, dba.admagrupousuario as grus
	WHERE	usua.usua_codigo	=	:Data 
	AND   usua.usua_codigo	=	grus.usua_codigo
	AND	grus.sist_codigo	=	:gstr_apl.codigosistema
	AND	grus.grpo_codigo	=	:ii_grupo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Usuarios")
	This.SetItem(Row, "usua_codigo", ls_Nula)
	
	RETURN 1
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Usuario ingresado no ha sido definido.~r~r" + &
					"Ingrese o Seleccione otro Código.")
	This.SetItem(Row, "usua_codigo", ls_Nula)
	
	RETURN 1
ELSEIF li_Grupo <> ii_Grupo THEN
	MessageBox("Atención", "Código de Usuario ingresado no pertenece a Grupo ." + &
					"is_GrupoNombre~r~rIngrese o Seleccione otro Código.")
	This.SetItem(Row, "usua_codigo", ls_Nula)
	
	RETURN 1
ELSE
	istr_mant.Argumento[2]	=	Data
	is_Usuario					=	Data
END IF
end event

event itemerror;RETURN 1
end event

type st_3 from statictext within w_mant_admausuaropcio_new
integer x = 1339
integer y = 56
integer width = 1783
integer height = 560
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

type dw_grupo from datawindow within w_mant_admausuaropcio_new
integer x = 1833
integer y = 340
integer width = 974
integer height = 92
integer taborder = 10
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_admagrupos"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Codigo, li_Nula

SetNull(li_Nula)

li_Codigo					=	Integer(Data)

SELECT	grpo_nombre
	INTO	:is_GrupoNombre
	FROM	dba.admagruposistema
	WHERE	grpo_codigo	=	:li_Codigo
	and	sist_codigo =	:gstr_apl.codigosistema;
	

IF SQLca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Grupos de Usuarios")
	This.SetItem(Row, "grpo_codigo", li_Nula)
	
	RETURN 1
ELSEIF SQLca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Grupo ingresado no ha sido definido.~r~r" + &
				  " Ingrese o Seleccione otro Grupo.")
	This.SetItem(Row, "grpo_codigo", li_Nula)	
	RETURN 1
ELSE
	ii_Grupo						=	li_Codigo
	istr_mant.Argumento[1]	=	Data
	
	IF rb_usuarios.Checked THEN	
		dw_usuario.Getchild("usua_codigo", idwc_usuarios)
		idwc_usuarios.Retrieve(li_Codigo,gstr_apl.codigosistema)
	ELSE
		dw_1.SetItem(il_Fila,"usop_soloco",'TODOS')
	END IF
END IF
end event

event itemerror;RETURN 1
end event


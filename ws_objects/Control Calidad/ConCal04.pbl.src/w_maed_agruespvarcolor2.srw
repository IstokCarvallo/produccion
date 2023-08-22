$PBExportHeader$w_maed_agruespvarcolor2.srw
forward
global type w_maed_agruespvarcolor2 from w_mant_encab_deta_csd
end type
end forward

global type w_maed_agruespvarcolor2 from w_mant_encab_deta_csd
integer width = 2368
integer height = 2124
string title = "AGRUPACION VARIEDAD"
string menuname = ""
event ue_imprimir ( )
end type
global w_maed_agruespvarcolor2 w_maed_agruespvarcolor2

type variables

uo_especie					iuo_especie
uo_variedades           iuo_variedades
uo_agruespvarcolor      iuo_agrupavari

Integer						ii_variedades
end variables

forward prototypes
public subroutine habilitaingreso (string columna)
public subroutine habilitaencab (boolean habilita)
public subroutine buscavariedad ()
public function integer buscaespecie ()
public subroutine existegrupo (integer ai_codigo, integer ai_especie)
public function boolean duplicado (integer ai_codigo, integer ai_especie, integer ai_variedad, string as_calibre)
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
String		ls_argumento, ls_argum1
str_info	lstr_info

lstr_info.titulo	= "Agrupación Calibres"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_agrupacionespecie"
vinf.dw_1.SetTransObject(sqlca)


fila = vinf.dw_1.Retrieve()

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE	
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Sort()
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)
end event

public subroutine habilitaingreso (string columna);Boolean	lb_estado = True

IF columna <> "agru_codigo" AND &
	(dw_2.GetItemNumber(1, "agru_codigo") = 0 OR IsNull(dw_2.GetItemNumber(1, "agru_codigo"))) THEN
	lb_estado = False
END IF

IF columna <> "espe_codigo" AND &
	(dw_2.GetItemNumber(1, "espe_codigo") = 0 OR IsNull(dw_2.GetItemNumber(1, "espe_codigo"))) THEN
	lb_estado = False
END IF
	
IF columna <> "agru_descri" AND &
	(dw_2.GetItemString(1, "agru_descri") = "" OR IsNull(dw_2.GetItemString(1, "agru_descri"))) THEN
	lb_estado = False
END IF
	
pb_ins_det.Enabled = lb_estado
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.SetTabOrder("agru_codigo",10)
	dw_2.Modify("agru_codigo.BackGround.Color = " + String(rgb(255,255,255)))
ELSE
	dw_2.SetTabOrder("agru_codigo",0)
	dw_2.Modify("agru_codigo.BackGround.Color = " + String(rgb(192,192,192)))
END IF
end subroutine

public subroutine buscavariedad ();str_busqueda		lstr_busq
lstr_busq.argum[1] = Istr_mant.argumento[1]
lstr_busq.argum[2] = Istr_mant.argumento[1]
OpenWithParm(w_busc_variedades, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[4] <> "" THEN
	IF NOT Duplicado(Integer(Istr_mant.argumento[2]),Integer(Istr_mant.argumento[1]),&
	                 Integer(lstr_busq.argum[4]), dw_1.Object.agru_calibr[il_fila]) THEN
				dw_1.Object.vari_codigo[il_fila] 	= 	Integer(lstr_busq.argum[4])
				dw_1.Object.vari_nombre[il_fila] 	=	lstr_busq.argum[5]
			END IF
END IF

RETURN 
end subroutine

public function integer buscaespecie ();str_busqueda		lstr_busq

OpenWithParm(w_busc_especies, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	//IF NOT Duplicado(Integer(lstr_busq.argum[1])) THEN
				dw_2.Object.espe_codigo[1] 	= 	Integer(lstr_busq.argum[1])
				dw_2.Object.espe_nombre[1] 	=	lstr_busq.argum[2]
				Istr_mant.argumento[1] = lstr_busq.argum[1]
			//END IF
END IF

RETURN 1
end function

public subroutine existegrupo (integer ai_codigo, integer ai_especie);Integer li_cantidad

SELECT count(*)
INTO   :li_cantidad
FROM   dba.ctlcalagrucalenca
WHERE  agru_codigo = :ai_codigo
AND    espe_codigo = :ai_especie;

		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Agrupación de Especies" )
ELSEIF li_cantidad > 0 THEN
	This.Triggerevent("ue_recuperadatos")
END IF 
end subroutine

public function boolean duplicado (integer ai_codigo, integer ai_especie, integer ai_variedad, string as_calibre);Long		ll_fila
Integer  li_codigo
	
ll_fila	= dw_1.Find("agru_codigo = " +  String(ai_codigo) 	+ ' AND ' + &
                     "espe_codigo = " +  String(ai_especie)	+ ' AND ' + &
                     "vari_codigo = " +  String(ai_variedad)+ ' AND ' + &
                     "agru_calibr = '"+  String(as_calibre)	+ "'", 1, dw_1.RowCount())	

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Esta Combinación " + &
							"[ "+ String(ai_variedad, '00') + " - " + as_calibre + " ] " +&
							"ya fue ingresada en esta Agrupación",Exclamation!, Ok!)
	RETURN True
END IF

 SELECT   agru_codigo
	INTO   :li_codigo
	FROM   dba.ctlcalagrucaldeta
	WHERE  espe_codigo = :ai_especie
	AND    vari_codigo = :ai_variedad
	AND	 agru_calibr =	:as_calibre
	USING  sqlca;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Detalle de Grupos agruespvarcolordeta")
	RETURN True
ELSEIF li_codigo > 0 THEN
	MessageBox("Error", "La Combinación "+&
							  "[ "+ String(ai_variedad, '00') + " - " + as_calibre + " ] " +&
							  "pertenece a la Agupación Nro. " + &
					        String(li_codigo, '00') , StopSign!)
	RETURN True
ELSE
	RETURN False
END IF
end function

event open;call super::open;buscar	= 	"Código:agec_codigo,Descripción:agec_descri"
ordenar	= 	"Código:agec_codigo,Descripción:agec_descri"

iuo_especie	   = CREATE uo_especie
iuo_variedades = CREATE uo_variedades
iuo_agrupavari = CREATE uo_agruespvarcolor


pb_nuevo.TriggerEvent(Clicked!)
end event

event ue_borra_detalle;call super::ue_borra_detalle;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

//This.TriggerEvent ("ue_validaborrar_detalle")
This.TriggerEvent ("ue_validaborrar")
IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

//OpenWithParm(iw_mantencion, istr_mant)
//
//istr_mant = Message.PowerObjectParm

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
		HabilitaEncab(True)
		pb_eli_det.Enabled = False
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_nuevo_detalle;istr_mant.borra	= False
istr_mant.agrega	= True

il_fila = dw_1.InsertRow(0)

dw_1.Object.agru_codigo[il_fila] = Integer(istr_mant.argumento[2])
dw_1.Object.espe_codigo[il_fila] = Integer(istr_mant.argumento[1])

IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SetColumn("vari_codigo")
dw_1.SetFocus()
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta
DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[1]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[1]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
				pb_imprimir.Enabled		= True
				pb_ins_det.Enabled	= True

				IF ll_fila_d > 0 THEN
					pb_eli_det.Enabled	= True
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
				ELSE
					pb_ins_det.SetFocus()
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_maed_agruespvarcolor2.create
call super::create
end on

on w_maed_agruespvarcolor2.destroy
call super::destroy
end on

event ue_seleccion;call super::ue_seleccion;str_busqueda	lstr_busq

OpenWithParm(w_busc_grupoespecie,lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	dw_1.Object.agru_codigo[1] 	= 	Integer(lstr_busq.argum[1])
	istr_mant.argumento[1] 			= 	lstr_busq.argum[1]
	This.TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_nuevo;call super::ue_nuevo;HabilitaEncab(True)
dw_2.setColumn("agru_codigo")
end event

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

IF dw_2.width > il_AnchoDw_1 THEN
	maximo		=	dw_2.width
ELSE
	dw_1.width	=	This.WorkSpaceWidth() - 400
	maximo		=	dw_1.width
END IF

dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					= 37

dw_1.x					= 37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.y					= 64 + dw_2.Height
dw_1.height				= This.WorkSpaceHeight() - dw_1.y - 41

//gb_1.x 					= This.WorkSpaceWidth() - 310
//gb_1.y 					= 5
//gb_1.width				= 275

li_posic_x				= This.WorkSpaceWidth() - 250
//li_posic_y				= gb_1.y + 88

IF pb_buscar.Visible THEN
	pb_buscar.x				= li_posic_x
	pb_buscar.y				= li_posic_y
	pb_buscar.width		= 156
	pb_buscar.height		= 133
	li_visible ++
	li_posic_y += 180
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				= li_posic_x
	pb_nuevo.y				= li_posic_y
	pb_nuevo.width			= 156
	pb_nuevo.height		= 133
	li_visible ++
	li_posic_y += 180
END IF

IF	pb_eliminar.Visible THEN
	pb_eliminar.x			= li_posic_x
	pb_eliminar.y			= li_posic_y
	pb_eliminar.width		= 156
	pb_eliminar.height	= 133
	li_visible ++
	li_posic_y += 180
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x				= li_posic_x
	pb_grabar.y				= li_posic_y
	pb_grabar.width		= 156
	pb_grabar.height		= 133
	li_visible ++
	li_posic_y += 180
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			= li_posic_x
	pb_imprimir.y			= li_posic_y
	pb_imprimir.width		= 156
	pb_imprimir.height	= 133
	li_visible ++
	li_posic_y += 180
END IF

IF pb_salir.Visible THEN
	pb_salir.x				= li_posic_x
	pb_salir.y				= li_posic_y
	pb_salir.width			= 156
	pb_salir.height		= 133
	li_visible ++
	li_posic_y += 180
END IF

//gb_1.height				= 180 * li_visible + 97 /*  (Según Botones Visibles)  */
//gb_2.x 					= gb_1.x
//gb_2.y 					= 1293
//gb_2.width				= 275
//gb_2.height				= 180 * 2 + 97 /*  (2 Botones)  */

pb_ins_det.x			= li_posic_x
//pb_ins_det.y			= gb_2.y + 93
pb_ins_det.width		= 156
pb_ins_det.height		= 133

pb_eli_det.x			= li_posic_x
pb_eli_det.y			= pb_ins_det.y + 180
pb_eli_det.width		= 156
pb_eli_det.height		= 133

//Integer	maximo
//
//maximo	= dw_1.width
//
//IF dw_2.width > maximo THEN maximo = dw_2.width
//
//dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
//dw_2.y					= 37
//dw_1.x					= 37 + Round((maximo - dw_1.width) / 2, 0)
//dw_1.y					= 64 + dw_2.Height
//dw_1.height			= This.WorkSpaceHeight() - dw_1.y - 41
//gb_1.width				= 275
//gb_1.height				= 985
//gb_1.x 					= This.WorkSpaceWidth() - 351
//gb_1.y 					= 40
//gb_2.width				= 275
//gb_2.height				= 449
//gb_2.x 					= gb_1.x
//gb_2.y 					= 1233
//pb_buscar.x			= This.WorkSpaceWidth() - 292
//pb_buscar.y			= 125
//pb_buscar.width		= 156
//pb_buscar.height		= 133
//pb_nuevo.x				= pb_buscar.x
//pb_nuevo.y				= 125
//pb_nuevo.width		= 156
//pb_nuevo.height		= 133
//pb_eliminar.x			= pb_buscar.x
//pb_eliminar.y			= 305
//pb_eliminar.width		= 156
//pb_eliminar.height		= 133
//pb_grabar.x			= pb_buscar.x
//pb_grabar.y			= 485
//pb_grabar.width		= 156
//pb_grabar.height		= 133
//pb_imprimir.x			= pb_buscar.x
//pb_imprimir.y			= 665
//pb_imprimir.width		= 156
//pb_imprimir.height		= 133
//pb_salir.x				= pb_buscar.x
//pb_salir.y				= 845
//pb_salir.width			= 156
//pb_salir.height			= 133
//pb_ins_det.x			= pb_buscar.x
//pb_ins_det.y			= 1321
//pb_ins_det.width		= 156
//pb_ins_det.height		= 133
//pb_eli_det.x			= pb_buscar.x
//pb_eli_det.y			= 1493
//pb_eli_det.width		= 156
//pb_eli_det.height		= 133
end event

event ue_modifica_detalle;call super::ue_modifica_detalle;//IF dw_1.RowCount() > 0 THEN
//	istr_mant.agrega	= False
//	istr_mant.borra	= False
//
//	OpenWithParm(iw_mantencion, istr_mant)
//END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long	ll_fila

FOR ll_fila = 1 TO dw_1.RowCount()
	
	IF dw_1.Object.espe_codigo[ll_fila] = 0 OR IsNull(dw_1.Object.espe_codigo[ll_fila]) THEN
		dw_1.DeleteRow(ll_fila)
		ll_fila = ll_fila -1
	END IF
	
NEXT
end event

event ue_validaborrar;IF MessageBox("Borrar Registro","Desea Borrar la Información ?", Question!, YesNo!) = 1 THEN
	istr_mant.respuesta = 1
ELSE
	istr_mant.respuesta = -1
END IF

RETURN 
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_agruespvarcolor2
integer x = 41
integer y = 692
integer width = 1829
integer height = 1148
integer taborder = 100
string title = "Detalle de Agrupación Variedad Calibre"
string dataobject = "dw_mues_agruespcalibre"
end type

event dw_1::itemchanged;call super::itemchanged;Integer	li_codigo, li_null
String	ls_columna

ls_columna = dwo.name
SetNull(li_null)
CHOOSE CASE ls_columna
	CASE "vari_codigo"
		IF NOT iuo_variedades.Existe(dw_2.Object.espe_codigo[1],integer(data), true, sqlca) THEN
			This.SetItem(row, "vari_nombre", String(li_null))
			This.SetItem(row, "vari_codigo", li_null)
			Return 1
		ELSE
			IF Duplicado(Integer(Istr_mant.argumento[2]),Integer(Istr_mant.argumento[1]),&
			             Integer(Data), dw_1.Object.agru_calibr[row]) THEN
				This.SetItem(row, "vari_nombre", String(li_null))
				This.SetItem(row, "vari_codigo", Integer(li_null))
				Return 1
			ELSE
				This.Object.vari_nombre[row]	=	iuo_variedades.NombreVariedad
				ii_variedades						=	Integer(Data)
				This.AcceptText()
			END IF
		END IF
		
	CASE "agru_calibr"
		IF Duplicado(Integer(Istr_mant.argumento[2]),Integer(Istr_mant.argumento[1]),&
						 ii_variedades, Data) THEN
			This.SetItem(row, "agru_calibr", String(li_null))
			Return 1
		ELSE
			This.AcceptText()
		END IF
		
END CHOOSE

HabilitaIngreso(ls_columna)
end event

event dw_1::clicked;//
end event

event dw_1::doubleclicked;//

end event

event dw_1::buttonclicked;call super::buttonclicked;String	ls_columna

ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "b_variedad"
		buscavariedad()
		
END CHOOSE

HabilitaIngreso(ls_columna)
end event

event dw_1::rowfocuschanged;//
end event

event dw_1::ue_seteafila;//
end event

event dw_1::getfocus;THIS.SelectRow(0,False)
//
end event

event dw_1::itemerror;Return 1
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_agruespvarcolor2
integer x = 128
integer y = 60
integer width = 1687
integer height = 628
string dataobject = "dw_mant_agruespvarcalibre"
boolean maxbox = true
end type

event dw_2::itemchanged;Integer	li_null
String	ls_columna

ls_columna = GetColumnName()
SetNull(li_null)
CHOOSE CASE ls_columna
	CASE "agru_codigo"
       Istr_mant.argumento[2] = Data
	
	CASE "espe_codigo"
		IF NOT iuo_especie.Existe(Integer(Data),True,SqlCa) THEN
			This.SetItem(1,ls_Columna,li_null)
			RETURN 1
		ELSE
			Istr_mant.argumento[1] = Data
			dw_2.Object.espe_nombre[1] = iuo_especie.nombre
			Existegrupo(Integer(Istr_mant.argumento[2]),Integer(Istr_mant.argumento[1]))
		END IF
END CHOOSE

HabilitaIngreso(ls_columna)
end event

event dw_2::clicked;//
end event

event dw_2::doubleclicked;//
end event

event dw_2::buttonclicked;call super::buttonclicked;String	ls_columna

ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "b_especie"
		buscaespecie()
		
END CHOOSE

end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_agruespvarcolor2
integer x = 1998
integer y = 124
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_agruespvarcolor2
integer x = 1998
integer y = 304
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_agruespvarcolor2
integer x = 1998
integer y = 488
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_agruespvarcolor2
integer x = 1998
integer y = 668
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_agruespvarcolor2
integer x = 1998
integer y = 844
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_agruespvarcolor2
integer x = 2007
integer y = 1296
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_agruespvarcolor2
integer x = 2007
integer y = 1472
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_agruespvarcolor2
integer x = 1998
end type


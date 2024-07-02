$PBExportHeader$w_consulta_lote_comercial.srw
$PBExportComments$Consulta por lote comercial entregando su detalle
forward
global type w_consulta_lote_comercial from w_mant_directo
end type
type dw_2 from datawindow within w_consulta_lote_comercial
end type
type dw_3 from datawindow within w_consulta_lote_comercial
end type
end forward

global type w_consulta_lote_comercial from w_mant_directo
integer width = 3936
integer height = 2024
string title = "Consulta del Lote"
dw_2 dw_2
dw_3 dw_3
end type
global w_consulta_lote_comercial w_consulta_lote_comercial

type variables
DataWindowChild	idwc_Especies, idwc_Productor
end variables

forward prototypes
public subroutine buscalote ()
end prototypes

public subroutine buscalote ();String  ls_Lote, ls_Null
Str_busqueda	lstr_busq

SetNull(ls_Null)

lstr_busq.argum[1] = String(gstr_paramplanta.codigoplanta)
lstr_busq.argum[2] = ''

OpenWithParm(w_busc_lotesfrutacomenc, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_3.SetColumn("lote")
	dw_3.SetFocus()
ELSE
	dw_3.Object.lofc_pltcod[1]	=	Integer(lstr_busq.argum[1])
	dw_3.Object.lofc_espcod[1]	=	Integer(lstr_busq.argum[2])
	dw_3.Object.lofc_lotefc[1]	=	Integer(lstr_busq.argum[3])
	
	Istr_Mant.Argumento[1]	=	lstr_busq.argum[1]
	Istr_Mant.Argumento[2]	=	lstr_busq.argum[2]
	Istr_Mant.Argumento[3]	=	lstr_busq.argum[3]

	Istr_Mant.Argumento[4]			=	String(dw_3.Object.lofc_pltcod[1],"0000") + &
												String(dw_3.Object.lofc_espcod[1],"00") + &
												String(dw_3.Object.lofc_lotefc[1],"00000000")

	dw_3.Object.lote[1] = Istr_Mant.Argumento[4]
	TriggerEvent("ue_recuperadatos")
	dw_3.SetColumn("lote")
	dw_3.SetFocus()
END IF

RETURN

end subroutine

on w_consulta_lote_comercial.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.dw_3
end on

on w_consulta_lote_comercial.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.dw_3)
end on

event open;call super::open;dw_2.SetTransObject(sqlca)

dw_3.GetChild("lofc_espcod",idwc_especies)
idwc_especies.SetTransObject(SQLCA)
idwc_especies.Retrieve(gstr_parempresa.empr_codexp)

dw_3.GetChild("prod_codigo",idwc_Productor)
idwc_Productor.SetTransObject(SQLCA)
idwc_Productor.Retrieve(-1)

dw_3.SetTransObject(sqlca)

dw_3.InsertRow(0)
dw_3.SetColumn("lote")

pb_lectura.enabled = False



end event

event closequery;//
end event

event ue_recuperadatos();call super::ue_recuperadatos;String ls_Null
Integer li_fila		

SetNull(ls_Null)

li_fila = dw_3.Retrieve(Integer(istr_Mant.Argumento[1]),Integer(istr_Mant.Argumento[2]), &
  					  		  Integer(istr_Mant.Argumento[3]))
pb_lectura.enabled = true
IF li_Fila <= 0 THEN
	dw_3.InsertRow(0)
	Return	
ELSEIF li_fila > 0 THEN
	dw_3.Object.lote[1] = Istr_Mant.Argumento[4]
END IF
end event

event resize;call super::resize;dw_2.Width	=	dw_1.Width
dw_2.Width	=	dw_1.Width
end event

type st_encabe from w_mant_directo`st_encabe within w_consulta_lote_comercial
boolean visible = false
integer x = 2674
integer y = 588
integer width = 229
integer height = 492
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_consulta_lote_comercial
integer x = 3319
integer y = 552
end type

event pb_nuevo::clicked;call super::clicked;dw_2.Reset()
dw_3.Reset()
dw_3.Insertrow(0)

dw_3.Object.lote.protect = 0
dw_3.SetColumn("lote")
pb_lectura.Enabled = True
end event

type pb_lectura from w_mant_directo`pb_lectura within w_consulta_lote_comercial
integer x = 3351
integer y = 124
end type

event pb_lectura::clicked;Integer li_Planta, li_Especie, li_folio


IF dw_1.Retrieve(Integer(istr_Mant.Argumento[1]),Integer(istr_Mant.Argumento[2]), &
                 Integer(istr_Mant.Argumento[3])) >= 0  OR &
	dw_2.Retrieve(Integer(istr_Mant.Argumento[1]),Integer(istr_Mant.Argumento[2]), &
                 Integer(istr_Mant.Argumento[3])) >= 0  THEN			
	pb_lectura.enabled = false
	dw_3.Object.lote.Protect = 1
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF

RETURN
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_consulta_lote_comercial
boolean visible = false
integer x = 3337
integer y = 1420
end type

type pb_insertar from w_mant_directo`pb_insertar within w_consulta_lote_comercial
boolean visible = false
integer x = 3333
integer y = 1212
end type

type pb_salir from w_mant_directo`pb_salir within w_consulta_lote_comercial
integer x = 3342
integer y = 1688
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_consulta_lote_comercial
boolean visible = false
integer x = 3323
integer y = 768
end type

type pb_grabar from w_mant_directo`pb_grabar within w_consulta_lote_comercial
boolean visible = false
integer x = 3328
integer y = 1008
end type

type dw_1 from w_mant_directo`dw_1 within w_consulta_lote_comercial
integer x = 82
integer y = 1108
integer width = 2944
integer height = 720
boolean titlebar = true
string title = "Detalle de Existencia"
string dataobject = "dw_con_spro_camaraexistecom"
end type

type dw_2 from datawindow within w_consulta_lote_comercial
integer x = 82
integer y = 468
integer width = 2944
integer height = 628
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "Detalle Recepción del Lote"
string dataobject = "dw_con_spro_lotesfrutacomdeta"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within w_consulta_lote_comercial
integer x = 133
integer y = 44
integer width = 2843
integer height = 412
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dw_con_spro_lotesfrutacomenca"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String ls_columna, ls_Null
Integer li_fila

Setnull(ls_Null)
dw_3.AcceptText()
ls_Columna = dwo.Name
Istr_Mant.Argumento[4] = Data

CHOOSE CASE ls_Columna
	CASE "lote"
      IF len(data) = 14 THEN
			istr_Mant.Argumento[1]	=	Mid(data,1,4)
			istr_Mant.Argumento[2]	=	Mid(data,5,2)
			istr_Mant.Argumento[3]	=	Mid(data,7,8)
			li_fila = dw_3.Retrieve(Integer(istr_Mant.Argumento[1]),Integer(istr_Mant.Argumento[2]), &
						  		 Integer(istr_Mant.Argumento[3]))
			pb_lectura.enabled = true
			IF li_Fila <= 0 THEN
				dw_3.InsertRow(0)
				MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
				dw_3.Object.lote[1] = ls_Null
				Return
			ELSE
				dw_3.Object.lote[1] = Istr_Mant.Argumento[4]
				dw_3.Object.lote.Protect = 1
			END IF
		ELSE
			MessageBox("Atención","La cantidad de digitos Ingresados no es Valida",Exclamation!,Ok!)
			dw_3.Object.lote[1] = ls_Null
			Return	
		END IF
END CHOOSE	
	
	
	
	
	
	
	
	
	
	
	
	
	

end event

event buttonclicked;CHOOSE CASE dwo.Name
	CASE "b_lote"
		buscalote()

END CHOOSE
end event

